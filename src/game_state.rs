use crate::constants::misc::{DARK_SQUARES, LIGHT_SQUARES};
use crate::mechanics::*;
use crate::modes::gui::BoardCoordinate;
use crate::movegen::MoveType;
use arrayvec::ArrayVec;
use macroquad::prelude::*;
use std::fmt::Display;
use std::time::Duration;

#[derive(Clone)]
pub struct GameState {
    pub bitboards: BitBoards,
    pub pins_and_checkers: PinsAndCheckers,
    pub side_to_move: PieceColour,
    pub castling_rights: CastlingRights,
    pub en_passant_square: Option<BitBoard>,
    pub halfmove_clock: u16,
    pub fullmove_number: u16,
    pub previous_state: StateCache,
}

#[derive(Clone)]
pub struct PreviousState {
    pub bitboards: BitBoards,
    pub pins_and_checkers: PinsAndCheckers,
    pub last_move: Move,
    pub castling_rights: CastlingRights,
    pub en_passant_square: Option<BitBoard>,
    pub halfmove_clock: u16,
}

type StateCache = Vec<PreviousState>;

pub type Seconds = f32;

#[derive(Debug, Clone, Copy)]
pub struct Clock {
    // seconds
    pub fixed_time: Seconds,
    pub increment: Seconds,
    pub white_time_left: Seconds,
    pub black_time_left: Seconds,
}

// core methods
impl GameState {
    pub fn from_fen(fen: &str) -> GameState {
        let mut bitboards = BitBoards::default();
        let mut square_index = 56;

        let fen_parts: Vec<&str> = fen.split_whitespace().collect();
        if fen_parts.len() != 6 {
            panic!("Invalid FEN format")
        }

        let board = fen_parts[0];
        let active_colour = fen_parts[1];
        let castling = fen_parts[2];
        let en_passant = fen_parts[3];
        let halfmove = fen_parts[4];
        let fullmove = fen_parts[5];

        for character in board.chars() {
            match character {
                '/' => {
                    square_index -= 16;
                    continue;
                } // new rank indicator
                '1'..='8' => square_index += character.to_digit(10).unwrap(),
                piece => {
                    let square_bit = 1u64 << square_index; // create a 64 bit 000...001, left shift the 1 to square position
                    let piece = Piece::from_fen_char(piece).unwrap();

                    bitboards.pieces[piece.colour as usize][piece.kind as usize] |= square_bit;
                    square_index += 1;
                }
            }
        }
        bitboards.recompute_aggregates();

        // side to move
        let side_to_move = match active_colour {
            "w" => PieceColour::White,
            "b" => PieceColour::Black,
            _ => panic!("Fen string side to move invalid"),
        };

        // pins and checks
        let pins_and_checkers = bitboards.get_pins_and_checks(side_to_move);

        // castling rights
        let castling_rights = CastlingRights {
            white_kingside: castling.contains('K'),
            white_queenside: castling.contains('Q'),
            black_kingside: castling.contains('k'),
            black_queenside: castling.contains('q'),
        };

        // en passant square
        let en_passant_square = match en_passant {
            "-" => None,
            square => {
                let file = square.chars().next().unwrap() as u8 - b'a';
                let rank = square.chars().nth(1).unwrap() as u8 - b'1';
                Some(1u64 << (rank * 8 + file))
            }
        };

        // move counters
        let halfmove_clock = halfmove.parse().unwrap();
        let fullmove_number = fullmove.parse().unwrap();

        // return
        GameState {
            bitboards,
            pins_and_checkers,
            side_to_move,
            castling_rights,
            en_passant_square,
            halfmove_clock,
            fullmove_number,
            previous_state: vec![],
        }
    }
    // sluggish old versions
    /*
        pub fn make_move(&mut self, move_to_make: Move) {
            // first cache state
            self.previous_state.push(PreviousState {
                bitboards: self.bitboards.clone(),
                pins_and_checkers: self.pins_and_checkers.clone(),
                last_move: move_to_make,
                castling_rights: self.castling_rights,
                en_passant_square: self.en_passant_square,
                halfmove_clock: self.halfmove_clock,
            });

            let start_bitboard = 1u64 << move_to_make.start_square;
            let end_bitboard = 1u64 << move_to_make.end_square;

            let piece_moved = move_to_make.piece_moved;
            let colour_moved = self.side_to_move;

            let move_type = move_to_make.move_type;

            // remove piece at start
            self.bitboards.pieces[colour_moved as usize][piece_moved as usize] ^= start_bitboard;

            if let Some(captured) = move_to_make.captured
                && move_type == MoveType::Normal
            {
                // remove captured piece
                self.bitboards.pieces[1 - (colour_moved as usize)][captured as usize] ^= end_bitboard;
            }

            if move_to_make.promotion.is_none() {
                // place piece
                self.bitboards.pieces[colour_moved as usize][piece_moved as usize] ^= end_bitboard;
            } else {
                // add promoted piece
                let promotion_piece = move_to_make
                    .promotion
                    .expect("Non-promotion move snuck into promotion logic");
                self.bitboards.pieces[colour_moved as usize][promotion_piece as usize] ^= end_bitboard;
            }

            if [MoveType::CastleKingside, MoveType::CastleQueenside].contains(&move_type) {
                // implement the rook move
                let (rook_start, rook_end) = match (colour_moved, move_type) {
                    (PieceColour::White, MoveType::CastleKingside) => (7, 5),
                    (PieceColour::Black, MoveType::CastleKingside) => (63, 61),
                    (PieceColour::White, MoveType::CastleQueenside) => (0, 3),
                    (PieceColour::Black, MoveType::CastleQueenside) => (56, 59),
                    _ => unreachable!("Non-castling move snuck into castling logic"),
                };

                // remove rook from original square and add to new square
                let rook_move_bb = (1u64 << rook_start) | (1u64 << rook_end);
                self.bitboards.pieces[colour_moved as usize][PieceKind::Rook as usize] ^= rook_move_bb;
            };

            if move_type == MoveType::EnPassant {
                // square to remove pawn from is the location of the double push
                // so 1 rank behind en_passant_square if white captures
                let captured_pawn_square = match colour_moved {
                    PieceColour::White => self.en_passant_square.unwrap() >> 8,
                    PieceColour::Black => self.en_passant_square.unwrap() << 8,
                };
                // remove captured pawn from opposition bitboard
                self.bitboards.pieces[1 - (colour_moved as usize)][0] ^= captured_pawn_square;
            }

            self.bitboards.recompute_aggregates();
            self.update_internal_state_params(&move_to_make);
        }

        pub fn unmake_move(&mut self) {
            let Some(previous_state) = self.previous_state.pop() else {
                return; // don't undo if nothing to undo
            };
            let side_to_move = self.side_to_move as usize;
            let side_just_moved = 1 - side_to_move;
            let last_move = previous_state.last_move;

            // bitboard stuff
            // add pieces back to original places on bitboards
            let start_bb = 1u64 << last_move.start_square;
            let end_bb = 1u64 << last_move.end_square;
            let piece = last_move.piece_moved;

            // remove piece from end square (promoted piece if promotion)
            match last_move.promotion {
                Some(promotion) => self.bitboards.pieces[side_just_moved][promotion as usize] ^= end_bb,
                None => self.bitboards.pieces[side_just_moved][piece as usize] ^= end_bb,
            }

            // reappend captured piece to end square, if any
            if last_move.move_type != MoveType::EnPassant {
                // if not en passant, captured piece goes on end square of the capturing move
                if let Some(captured) = last_move.captured {
                    self.bitboards.pieces[side_to_move][captured as usize] ^= end_bb;
                }
            } else {
                // place en passant captured pawn on previous gamestate's en_passant_square
                let prev_ep_square = previous_state.en_passant_square.unwrap();
                let original_square = match prev_ep_square > 1u64 << 32 {
                    false => prev_ep_square << 8, // rank above ep square if white's side of the board
                    true => prev_ep_square >> 8,  // rank below ep square if black's side of board
                };
                self.bitboards.pieces[side_to_move][0] ^= original_square;
            }

            self.bitboards.pieces[side_just_moved][piece as usize] ^= start_bb; // add piece to start square

            if [MoveType::CastleKingside, MoveType::CastleQueenside].contains(&last_move.move_type) {
                let (rook_before_castling, rook_after_castling) =
                    match (side_just_moved, last_move.move_type) {
                        (0, MoveType::CastleKingside) => (7, 5),
                        (1, MoveType::CastleKingside) => (63, 61),
                        (0, MoveType::CastleQueenside) => (0, 3),
                        (1, MoveType::CastleQueenside) => (56, 59),
                        _ => unreachable!("Non-castling undo move snuck into castling logic"),
                    };
                // remove rook from square
                self.bitboards.pieces[side_just_moved][3] ^= 1u64 << rook_after_castling;
                // reappend to corner square
                self.bitboards.pieces[side_just_moved][3] ^= 1u64 << rook_before_castling;
            }

            // other calculated gamestate fields
            self.side_to_move = PieceColour::try_from(side_just_moved).unwrap();
            self.bitboards.recompute_aggregates();
            self.pins_and_checkers = self.bitboards.get_pins_and_checks(self.side_to_move);

            self.fullmove_number -= side_just_moved as u16; // only reduce if black just moved

            // reset cached state
            self.castling_rights = previous_state.castling_rights;
            self.halfmove_clock = previous_state.halfmove_clock;
            self.en_passant_square = previous_state.en_passant_square;

            // self.previous_state already had most recent move etc removed from vector by .pop() called previously
        }
    */
}

impl Clock {
    pub fn new(fixed_time: Seconds, increment: Seconds) -> Self {
        Clock {
            fixed_time,
            increment,
            white_time_left: fixed_time,
            black_time_left: fixed_time,
        }
    }
    pub fn tick(&mut self, side_to_move: PieceColour) {
        // macroquad-only function. use update for synchronous modes
        let frame_time = get_frame_time();
        match side_to_move {
            PieceColour::White => self.white_time_left -= frame_time,
            PieceColour::Black => self.black_time_left -= frame_time,
        }
    }

    pub fn update(&mut self, side_to_update: PieceColour, thought_time: Duration) {
        let thought_seconds = thought_time.as_secs_f32();
        match side_to_update {
            PieceColour::White => self.white_time_left -= thought_seconds,
            PieceColour::Black => self.black_time_left -= thought_seconds,
        }
    }

    pub fn increment(&mut self, side_to_move: PieceColour) {
        match side_to_move {
            PieceColour::White => self.white_time_left += self.increment,
            PieceColour::Black => self.black_time_left += self.increment,
        }
    }

    pub fn time_remaining(&self, side_to_move: PieceColour) -> Seconds {
        match side_to_move {
            PieceColour::White => self.white_time_left,
            PieceColour::Black => self.black_time_left,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum GameEnding {
    WhiteWins,
    BlackWins,
    Draw,
}

impl Display for GameEnding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GameEnding::WhiteWins => write!(f, "White wins."),
            GameEnding::BlackWins => write!(f, "Black wins."),
            GameEnding::Draw => write!(f, "The game is a draw."),
        }
    }
}

// game endings
impl GameState {
    pub fn out_of_time_ending(&self, clock: &Clock) -> Option<GameEnding> {
        let side_to_move = self.side_to_move;

        if clock.time_remaining(side_to_move) > 0.0 {
            return None;
        };

        // if your opponent's out of time but you've only got a king, you don't win
        if self.insufficient_material_to_win(side_to_move.flip()) {
            return Some(GameEnding::Draw);
        }

        match side_to_move {
            PieceColour::White => Some(GameEnding::BlackWins),
            PieceColour::Black => Some(GameEnding::WhiteWins),
        }
    }

    pub fn is_game_over(&self, legal_moves: &Moves) -> Option<GameEnding> {
        let exciting_endings = self.victory_or_stalemate(legal_moves);

        if exciting_endings.is_some() {
            return exciting_endings;
        }

        if self.draw_by_insufficient_material() {
            println!("suspect");
            return Some(GameEnding::Draw);
        }
        if self.draw_by_threefold_repetition() {
            return Some(GameEnding::Draw);
        }
        if self.draw_by_halfmove_clock() {
            return Some(GameEnding::Draw);
        }
        if self.draw_by_stupidly_long_game() {
            return Some(GameEnding::Draw);
        }

        None
    }

    fn victory_or_stalemate(&self, legal_moves: &Moves) -> Option<GameEnding> {
        if !legal_moves.is_empty() {
            return None;
        };

        if self.pins_and_checkers.check_mask == !0 {
            Some(GameEnding::Draw) // stalemate
        } else {
            match self.side_to_move {
                PieceColour::White => Some(GameEnding::BlackWins),
                PieceColour::Black => Some(GameEnding::WhiteWins),
            }
        }
    }

    fn insufficient_material_to_win(&self, side: PieceColour) -> bool {
        if self.bitboards.pieces[side as usize][0] != 0 {
            return false;
        }; // pawns
        if self.bitboards.pieces[side as usize][3] != 0 {
            return false;
        }; // rooks
        if self.bitboards.pieces[side as usize][4] != 0 {
            return false;
        }; // queens

        let knights = self.bitboards.pieces[side as usize][1];

        if knights.count_ones() > 1 {
            return false;
        }; // two knights is sufficient (although not with best play)

        let bishops = self.bitboards.pieces[side as usize][2];

        if (knights != 0) && (bishops != 0) {
            return false;
        }; // knight and bishop is sufficient

        let light_square_bishops = bishops & LIGHT_SQUARES;
        let dark_square_bishops = bishops & DARK_SQUARES;

        if (light_square_bishops != 0) && (dark_square_bishops != 0) {
            return false;
        }; // opposite bishops is sufficient

        true
    }

    fn draw_by_stupidly_long_game(&self) -> bool {
        // TODO not a real rule but worth preventing glitched loops
        self.fullmove_number >= 200
    }

    fn draw_by_halfmove_clock(&self) -> bool {
        self.halfmove_clock >= 50
    }

    fn draw_by_insufficient_material(&self) -> bool {
        self.insufficient_material_to_win(PieceColour::White)
            && self.insufficient_material_to_win(PieceColour::Black)
    }

    fn draw_by_threefold_repetition(&self) -> bool {
        // too expensive without hashing. need to keep a hash
        false
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Move {
    pub start_square: usize,
    pub end_square: usize,
    pub piece_moved: PieceKind,
    pub captured: Option<PieceKind>,
    pub promotion: Option<PieceKind>,
    pub move_type: MoveType,
}

impl Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // not really proper notation. tweak this for UCI
        let piece_char = match self.piece_moved {
            PieceKind::Pawn => "",
            PieceKind::Knight => "N",
            PieceKind::Bishop => "B",
            PieceKind::Rook => "R",
            PieceKind::Queen => "Q",
            PieceKind::King => "K",
        };

        if self.move_type == MoveType::CastleKingside {
            write!(f, "O-O")?;
        }
        if self.move_type == MoveType::CastleQueenside {
            write!(f, "O-O-O")?;
        }

        let start_square_name = BoardCoordinate::from_usize(self.start_square).square_name();
        let end_square_name = BoardCoordinate::from_usize(self.end_square).square_name();

        if self.captured.is_some() {
            write!(f, "{piece_char}{start_square_name}x{end_square_name}")
        } else {
            write!(f, "{piece_char}{start_square_name}{end_square_name}")
        }
    }
}

pub type Moves = ArrayVec<Move, 218>;

impl Move {
    pub fn quiet(start: usize, end: usize, piece_kind: PieceKind) -> Move {
        Move {
            start_square: start,
            end_square: end,
            piece_moved: piece_kind,
            captured: None,
            promotion: None,
            move_type: MoveType::Normal,
        }
    }

    pub fn capture(
        start: usize,
        end: usize,
        piece_kind: PieceKind,
        target_kind: PieceKind,
    ) -> Move {
        Move {
            start_square: start,
            end_square: end,
            piece_moved: piece_kind,
            captured: Some(target_kind),
            promotion: None,
            move_type: MoveType::Normal,
        }
    }

    pub fn promotion(
        start: usize,
        end: usize,
        promotion_choice: PieceKind,
        target_kind: Option<PieceKind>,
    ) -> Move {
        Move {
            start_square: start,
            end_square: end,
            piece_moved: PieceKind::Pawn,
            captured: target_kind,
            promotion: Some(promotion_choice),
            move_type: MoveType::Normal,
        }
    }
    pub fn promotions(start: usize, end: usize, target_kind: Option<PieceKind>) -> Moves {
        let promotions = [
            PieceKind::Knight,
            PieceKind::Bishop,
            PieceKind::Rook,
            PieceKind::Queen,
        ];
        promotions
            .into_iter()
            .map(|promotion_choice| Move::promotion(start, end, promotion_choice, target_kind))
            .collect()
    }

    pub fn en_passant(start: usize, end: usize) -> Move {
        Move {
            start_square: start,
            end_square: end,
            piece_moved: PieceKind::Pawn,
            captured: Some(PieceKind::Pawn),
            promotion: None,
            move_type: MoveType::EnPassant,
        }
    }

    pub fn castling(end: usize, castling_side: MoveType) -> Move {
        let start = match end {
            2 | 6 => 4,
            58 | 62 => 60,
            _ => unreachable!("invalid target square for castling"),
        };

        Move {
            start_square: start,
            end_square: end,
            piece_moved: PieceKind::King,
            captured: None,
            promotion: None,
            move_type: castling_side,
        }
    }
}
