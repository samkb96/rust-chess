use crate::constants::misc::{DARK_SQUARES, LIGHT_SQUARES};
use crate::mechanics::PieceColour::{Black, White};
use crate::mechanics::PieceKind::{Bishop, King, Knight, Pawn, Queen, Rook};
use crate::mechanics::*;
use crate::movegen::MoveType;
use macroquad::prelude::*;
use smallvec::SmallVec;
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
    pub pieces: [Option<Piece>; 32],
    pub piece_locations: [Option<u8>; 32], // locations indexed by piece_id
    pub id_squares: [Option<u8>; 64] // ids indexed by location
}

#[derive(Clone)]
pub struct PreviousState {
    pub bitboards: BitBoards,
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

/// fen methods
impl GameState {
    pub fn from_fen(fen: &str) -> GameState {
        let mut bitboards = BitBoards::default();
        let mut square_index = 56; // start on a8, move up files, and jump back at new rank character 

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

        let mut pieces: [Option<Piece>; 32] = [None; 32];
        let mut piece_locations: [Option<u8>; 32] = [None; 32];
        let mut id_squares: [Option<u8>; 64] = [None; 64];

        let mut next_white_id = 0u8;
        let mut next_black_id = 16u8;

        for character in board.chars() {
            match character {
                '/' => {
                    // jump from h file to a file, move one rank back
                    square_index -= 16;
                    continue;
                } 
                '1'..='8' => square_index += character.to_digit(10).unwrap() as usize,
                piece => {
                    let square_bit = 1u64 << square_index; // create a 64 bit 000...001, left shift the 1 to square position
                    let piece_kind = match piece.to_ascii_lowercase() {
                        'p' => Pawn,
                        'n' => Knight,
                        'b' => Bishop,
                        'r' => Rook,
                        'q' => Queen,
                        'k' => King,
                        _ => panic!("Invalid FEN")
                    };

                    let (piece_colour, piece_id) = if piece.is_ascii_lowercase() {
                        (Black, next_black_id)
                    } else {
                        (White, next_white_id)
                    };

                    if piece_colour == White {
                        next_white_id += 1
                    } else {
                        next_black_id += 1
                    };

                    pieces[piece_id as usize] = Some(Piece { kind: piece_kind, colour: piece_colour, id: piece_id });
                    piece_locations[piece_id as usize] = Some(square_index as u8);
                    id_squares[square_index as usize] = Some(piece_id);

                    bitboards.pieces[piece_colour as usize][piece_kind as usize] |= square_bit;
                    square_index += 1;
                },

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
            pieces,
            piece_locations,
            id_squares,
        }
    }

    pub fn to_fen(&self) -> String {
        let mut fen = String::new();

        for rank in (0..8).rev() {
            let mut empty_count = 0;

            for file in 0..8 {
                let square = rank * 8 + file;
                let square_bit = 1u64 << square;

                if let Some(piece_char) = self.get_piece_char(square_bit) {
                    if empty_count > 0 {
                        fen.push_str(&empty_count.to_string());
                        empty_count = 0;
                    }
                    fen.push(piece_char);
                } else {
                    empty_count += 1;
                }
            }

            if empty_count > 0 {
                fen.push_str(&empty_count.to_string());
            }

            if rank > 0 {
                fen.push('/');
            }
        }

        fen.push(' ');
        fen.push(match self.side_to_move {
            PieceColour::White => 'w',
            PieceColour::Black => 'b',
        });

        fen.push(' ');
        let mut castling_fen = String::new();
        if self.castling_rights.white_kingside {
            castling_fen.push('K');
        }
        if self.castling_rights.white_queenside {
            castling_fen.push('Q');
        }
        if self.castling_rights.black_kingside {
            castling_fen.push('k');
        }
        if self.castling_rights.black_queenside {
            castling_fen.push('q');
        }

        fen.push_str(if castling_fen.is_empty() {
            "-"
        } else {
            &castling_fen
        });

        fen.push(' ');
        match self.en_passant_square {
            Some(ep_bb) if ep_bb != 0 => {
                let square = ep_bb.trailing_zeros() as usize;
                let file = (b'a' + (square % 8) as u8) as char;
                let rank = (b'1' + (square / 8) as u8) as char;
                fen.push(file);
                fen.push(rank);
            }
            _ => fen.push('-'),
        }

        fen.push(' ');
        fen.push_str(&self.halfmove_clock.to_string());

        fen.push(' ');
        fen.push_str(&self.fullmove_number.to_string());

        fen
    }

    fn get_piece_char(&self, square_bit: u64) -> Option<char> {
        for piece_kind in 0..6 {
            if (self.bitboards.pieces[0][piece_kind] & square_bit) != 0 {
                return Some(match piece_kind {
                    0 => 'P',
                    1 => 'N',
                    2 => 'B',
                    3 => 'R',
                    4 => 'Q',
                    5 => 'K',
                    _ => unreachable!(),
                });
            }
        }

        for piece_kind in 0..6 {
            if (self.bitboards.pieces[1][piece_kind] & square_bit) != 0 {
                return Some(match piece_kind {
                    0 => 'p',
                    1 => 'n',
                    2 => 'b',
                    3 => 'r',
                    4 => 'q',
                    5 => 'k',
                    _ => unreachable!(),
                });
            }
        }
        None
    }
}

impl GameState {
    pub fn piece_at_square(&self, square: usize) -> Option<Piece> {
        let id_at_square = self.id_squares[square]?;
        self.pieces[id_at_square as usize]
    }
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
pub struct OldMove {
    pub start_square: usize,
    pub end_square: usize,
    pub piece_moved: PieceKind,
    pub captured: Option<PieceKind>,
    pub promotion: Option<PieceKind>,
    pub move_type: MoveType,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
/// fast 32-bit encoding
pub struct Move {
    pub data: u32,
}

/// getters
impl Move {
    /// bits 0-5
    #[inline]
    pub fn start_square(self) -> usize {
        (self.data & 0b111111) as usize
    }

    /// bits 6-11
    #[inline]
    pub fn end_square(self) -> usize {
        ((self.data >> 6) & 0b111111) as usize
    }

    // TODO return u8 instead
    #[inline]
    pub fn piece_moved(self) -> PieceKind {
        PieceKind::try_from(((self.data >> 12) & 0b111) as u8).unwrap()
    }

    // TODO return u8 instead
    #[inline]
    pub fn captured(self) -> Option<PieceKind> {
        // host none as 0b111, else direct piece index
        let captured_bits = ((self.data >> 15) & 0b111) as u8;
        if captured_bits == 7 {
            None
        } else {
            Some(PieceKind::try_from(captured_bits).unwrap())
        }
    }

    // TODO return u8 instead
    #[inline]
    pub fn promotion_choice(self) -> Option<PieceKind> {
        let promotion_choice = ((self.data >> 18) & 0b111) as u8;
        if promotion_choice == 0 {
            None
        } else {
            Some(PieceKind::try_from(promotion_choice).unwrap())
        }
    }

    #[inline]
    pub fn move_type(self) -> MoveType {
        MoveType::try_from(((self.data >> 21) & 0b11) as u8).unwrap()
    }

    #[inline]
    pub fn side_moving(self) -> PieceColour {
        match (self.data >> 22) & 0b1 {
            0 => PieceColour::White,
            1 => PieceColour::Black,
            _ => unreachable!(),
        }
    }

    #[inline]
    pub fn id_moved(self) -> u8 {
        let moving_side = ((self.data >> 23) & 0b1) as u8;
        let moved_id = ((self.data >> 24) & 0b1111) as u8;
        moved_id + (16 * moving_side)
    }

    #[inline]
    pub fn id_captured(self) -> u8 {
        let moving_side = ((self.data >> 23) & 0b1) as u8;
        let captured_id = ((self.data >> 28) & 0b1111) as u8;
        captured_id + (16 * moving_side)
    }
}

/// setters
impl Move {
    // TODO everything needs rewriting to make use of smaller data without casts
    fn from_data(
        start_bits: u32,
        end_bits: u32,
        piecemoved_bits: u32,
        targetkind_bits: u32,
        promotion_bits: u32,
        move_type_bits: u32,
    ) -> Self {
        let data = start_bits
            | end_bits << 6
            | piecemoved_bits << 12
            | targetkind_bits << 15
            | promotion_bits << 18
            | move_type_bits << 21;

        Self { data }
    }

    pub fn quiet(start: usize, end: usize, piece_moved: PieceKind) -> Self {
        let start_bits = start as u32;
        let end_bits = end as u32;
        let piecemoved_bits = piece_moved as u32;
        Self::from_data(start_bits, end_bits, piecemoved_bits, 7, 0, 0)
    }

    pub fn capture(
        start: usize,
        end: usize,
        piece_moved: PieceKind,
        target_kind: PieceKind,
    ) -> Self {
        let start_bits = start as u32;
        let end_bits = end as u32;
        let piecemoved_bits = piece_moved as u32;
        let targetkind_bits = target_kind as u32;
        Self::from_data(start_bits, end_bits, piecemoved_bits, targetkind_bits, 0, 0)
    }

    pub fn promotion(
        start: usize,
        end: usize,
        promotion: PieceKind,
        target_kind: Option<PieceKind>,
    ) -> Self {
        let start_bits = start as u32;
        let end_bits = end as u32;
        // don't need piecekind bits as pawns are 0
        let targetkind_bits = if let Some(target_kind) = target_kind {
            target_kind as u32
        } else {
            0b111
        };
        let promotion_bits = promotion as u32;
        Self::from_data(start_bits, end_bits, 0, targetkind_bits, promotion_bits, 0)
    }

    pub fn promotions(
        start: usize,
        end: usize,
        target_kind: Option<PieceKind>,
    ) -> SmallVec<[Self; 4]> {
        let start_bits = start as u32;
        let end_bits = end as u32;
        // don't need piecekind bits as pawns are 0
        let targetkind_bits = if let Some(target_kind) = target_kind {
            target_kind as u32
        } else {
            0b111
        };
        (1u32..=4)
            .map(|promotion_bits| {
                Self::from_data(start_bits, end_bits, 0, targetkind_bits, promotion_bits, 0)
            })
            .collect()
    }

    pub fn en_passant(start: usize, end: usize) -> Self {
        let start_bits = start as u32;
        let end_bits = end as u32;
        let movetype_bits = 1u32;
        Self::from_data(start_bits, end_bits, 0, 0, 0, movetype_bits)
    }

    pub fn castling(end: usize, castling_side: u8) -> Self {
        let end_bits = end as u32;
        let start_bits = match end {
            2 | 6 => 4u32,
            58 | 62 => 60u32,
            _ => unreachable!("invalid target square for castling"),
        };

        let movetype_bits = castling_side as u32;

        Self::from_data(start_bits, end_bits, 5, 7, 0, movetype_bits)
    }
}

pub type Moves = SmallVec<[Move; 64]>;

impl OldMove {
    pub fn quiet(start: usize, end: usize, piece_kind: PieceKind) -> Self {
        Self {
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
    ) -> Self {
        Self {
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
    ) -> Self {
        Self {
            start_square: start,
            end_square: end,
            piece_moved: PieceKind::Pawn,
            captured: target_kind,
            promotion: Some(promotion_choice),
            move_type: MoveType::Normal,
        }
    }
    pub fn promotions(
        start: usize,
        end: usize,
        target_kind: Option<PieceKind>,
    ) -> SmallVec<[Self; 4]> {
        let promotions = [
            PieceKind::Knight,
            PieceKind::Bishop,
            PieceKind::Rook,
            PieceKind::Queen,
        ];
        promotions
            .into_iter()
            .map(|promotion_choice| Self::promotion(start, end, promotion_choice, target_kind))
            .collect()
    }

    pub fn en_passant(start: usize, end: usize) -> Self {
        Self {
            start_square: start,
            end_square: end,
            piece_moved: PieceKind::Pawn,
            captured: Some(PieceKind::Pawn),
            promotion: None,
            move_type: MoveType::EnPassant,
        }
    }

    pub fn castling(end: usize, castling_side: MoveType) -> Self {
        let start = match end {
            2 | 6 => 4,
            58 | 62 => 60,
            _ => unreachable!("invalid target square for castling"),
        };

        Self {
            start_square: start,
            end_square: end,
            piece_moved: PieceKind::King,
            captured: None,
            promotion: None,
            move_type: castling_side,
        }
    }
}
