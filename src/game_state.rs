
use crate::mechanics::*;
use crate::movegen::{MoveType};
use arrayvec::ArrayVec;
use macroquad::prelude::*;
use std::fmt::Display;


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

#[derive(Clone, PartialEq, Eq)]
pub struct PreviousState {
    last_move: Move,
    castling_rights: CastlingRights,
    en_passant_square: Option<BitBoard>,
    halfmove_clock: u16,
}

type StateCache = Vec<PreviousState>;

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

    pub fn make_move(&mut self, move_to_make: Move) {
        // first cache state
        self.previous_state.push(PreviousState {
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

    fn update_internal_state_params(&mut self, move_made: &Move) {
        // if move not capture & not pawn move, add 1 to halfmove clock
        if move_made.captured.is_none() & !(move_made.piece_moved == PieceKind::Pawn) {
            self.halfmove_clock += 1;
        }

        // castling rights lost if rook moves, king moves, rook captured
        if move_made.piece_moved == PieceKind::King {
            match self.side_to_move {
                PieceColour::White => {
                    self.castling_rights.white_kingside = false;
                    self.castling_rights.white_queenside = false;
                }
                PieceColour::Black => {
                    self.castling_rights.black_kingside = false;
                    self.castling_rights.black_queenside = false;
                }
            }
        }

        if move_made.piece_moved == PieceKind::Rook {
            match move_made.start_square {
                0 => {
                    self.castling_rights.white_queenside = false;
                }
                7 => {
                    self.castling_rights.white_kingside = false;
                }
                56 => {
                    self.castling_rights.black_queenside = false;
                }
                63 => {
                    self.castling_rights.black_kingside = false;
                }
                _ => (),
            };
        }

        // not super slick but works
        if move_made.captured.unwrap_or(PieceKind::Pawn) == PieceKind::Rook {
            match move_made.end_square {
                0 => {
                    self.castling_rights.white_queenside = false;
                }
                7 => {
                    self.castling_rights.white_kingside = false;
                }
                56 => {
                    self.castling_rights.black_queenside = false;
                }
                63 => {
                    self.castling_rights.black_kingside = false;
                }
                _ => (),
            }
        }

        // en passant square
        if move_made.piece_moved == PieceKind::Pawn {
            let (start, end) = (move_made.start_square, move_made.end_square);
            // should only be true if pawn has moved two squares
            if end.abs_diff(start) == 16 {
                // set en_passant_square to be the midpoint of the start and end moves
                self.en_passant_square = Some(1u64 << ((start + end) / 2))
            } else {
                self.en_passant_square = None
            }
        } else {
            self.en_passant_square = None
        }

        // pass turn over & update fullmove_number if black has just moved
        match self.side_to_move {
            PieceColour::White => {
                self.side_to_move = PieceColour::Black;
            }
            PieceColour::Black => {
                self.fullmove_number += 1;
                self.side_to_move = PieceColour::White;
            }
        }

        // recalculate pins and checks
        self.pins_and_checkers = self.bitboards.get_pins_and_checks(self.side_to_move);
    }
}

pub type Seconds = f32;

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
        let frame_time = get_frame_time();
        match side_to_move {
            PieceColour::White => self.white_time_left -= frame_time,
            PieceColour::Black => self.black_time_left -= frame_time,
        }
    }

    pub fn increment(&mut self, side_to_move: PieceColour) {
        match side_to_move {
            PieceColour::White => self.white_time_left += self.increment,
            PieceColour::Black => self.black_time_left += self.increment,
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
    pub fn is_game_over(&self, legal_moves: &Moves) -> Option<GameEnding> {
        let exciting_endings = self.victory_or_stalemate(legal_moves);
        if exciting_endings.is_some() {
            return exciting_endings;
        }

        if self.draw_by_insufficient_material() {
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

    fn draw_by_stupidly_long_game(&self) -> bool {
        // TODO not a real rule but worth preventing glitched loops
        self.fullmove_number >= 200
    }

    fn draw_by_halfmove_clock(&self) -> bool {
        self.halfmove_clock >= 50
    }

    fn draw_by_insufficient_material(&self) -> bool {
        // going by 'possible checkmate' version rather than 'forced checkmate'

        // any pawns, rooks, queens -> not a draw
        let pawns = self.bitboards.pieces[0][3] | self.bitboards.pieces[1][3];
        if pawns != 0 {
            return false;
        }
        let rooks = self.bitboards.pieces[0][3] | self.bitboards.pieces[1][3];
        if rooks != 0 {
            return false;
        }
        let queens = self.bitboards.pieces[0][4] | self.bitboards.pieces[1][4];
        if queens != 0 {
            return false;
        }

        // deal with the particulars of knights & bishop combinations
        // two knights, knight and bishop, opposite colour bishops all sufficient

        let light_squares = 0x55AA55AA55AA55AAu64;
        let dark_squares = 0xAA55AA55AA55AA55u64;

        for side in [0usize, 1usize] {
            let knights = self.bitboards.pieces[side][1];
            if knights.count_ones() > 1 {
                return false;
            }; // two knights

            let bishops = self.bitboards.pieces[side][2];
            let light_square_bishops = bishops & light_squares;
            let dark_square_bishops = bishops & dark_squares;

            if (knights != 0) && (bishops != 0) {
                return false;
            }; // knight and bishop
            if (light_square_bishops != 0) && (dark_square_bishops != 0) {
                return false;
            }; // opposite bishops
        }

        true
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

