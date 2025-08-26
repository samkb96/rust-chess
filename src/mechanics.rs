use crate::game::*;
use arrayvec::ArrayString;
use macroquad::prelude::*;

const PIECE_ID_TO_FEN: [char; 6] = ['p', 'n', 'b', 'r', 'q', 'k'];
pub const PIECE_KINDS: [PieceKind; 6] = [
    PieceKind::Pawn,
    PieceKind::Knight,
    PieceKind::Bishop,
    PieceKind::Rook,
    PieceKind::Queen,
    PieceKind::King,
];
pub const PIECE_COLOURS: [PieceColour; 2] = [PieceColour::White, PieceColour::Black];

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum PieceColour {
    White = 0,
    Black = 1,
}
impl TryFrom<usize> for PieceColour {
    type Error = ();

    fn try_from(colour_index: usize) -> Result<Self, Self::Error> {
        match colour_index {
            0 => Ok(PieceColour::White),
            1 => Ok(PieceColour::Black),
            _ => Err(()),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum PieceKind {
    Pawn = 0,
    Knight = 1,
    Bishop = 2,
    Rook = 3,
    Queen = 4,
    King = 5,
}

impl TryFrom<usize> for PieceKind {
    type Error = ();

    fn try_from(kind_index: usize) -> Result<Self, Self::Error> {
        match kind_index {
            0 => Ok(PieceKind::Pawn),
            1 => Ok(PieceKind::Knight),
            2 => Ok(PieceKind::Bishop),
            3 => Ok(PieceKind::Rook),
            4 => Ok(PieceKind::Queen),
            5 => Ok(PieceKind::King),
            _ => Err(()),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Piece {
    pub kind: PieceKind,
    pub colour: PieceColour,
}
impl Piece {
    pub fn from_fen_char(c: char) -> Option<Piece> {
        let piece_colour = if c.is_uppercase() {
            PieceColour::Black
        } else {
            PieceColour::White
        };
        let piece_kind = match c.to_ascii_lowercase() {
            'p' => PieceKind::Pawn,
            'n' => PieceKind::Knight,
            'b' => PieceKind::Bishop,
            'r' => PieceKind::Rook,
            'q' => PieceKind::Queen,
            'k' => PieceKind::King,
            _ => return None,
        };
        Some(Piece {
            kind: piece_kind,
            colour: piece_colour,
        })
    }

    pub fn to_fen_char(self) -> char {
        let fen_char = PIECE_ID_TO_FEN[self.kind as usize];
        match self.colour {
            PieceColour::White => fen_char,
            PieceColour::Black => fen_char.to_ascii_uppercase(),
        }
    }

    pub fn to_image_name(self) -> ArrayString<2> {
        // use arraystring to avoid heap allocation
        let mut image_name = ArrayString::<2>::new();
        let colour_prefix = match self.colour {
            PieceColour::White => 'w',
            PieceColour::Black => 'b',
        };
        image_name.push(colour_prefix);
        image_name.push(PIECE_ID_TO_FEN[self.kind as usize].to_ascii_uppercase());
        image_name
    }
}

#[derive(Clone)]
struct CastlingRights {
    white_kingside: bool,
    white_queenside: bool,
    black_kingside: bool,
    black_queenside: bool,
}
impl CastlingRights {
    fn initialise() -> Self {
        CastlingRights {
            white_kingside: true,
            white_queenside: true,
            black_kingside: true,
            black_queenside: true,
        }
    }
}

#[derive(Clone)]
pub struct GameState {
    pub bitboards: BitBoards,
    side_to_move: PieceColour,
    castling_rights: CastlingRights,
    en_passant_square: Option<u64>,
    halfmove_clock: u8,
    fullmove_number: u16,
}

impl GameState {
    pub fn initialise() -> Self {
        GameState {
            bitboards: BitBoards::from_fen(STARTING_POSITION_FEN),
            side_to_move: PieceColour::White,
            castling_rights: CastlingRights::initialise(),
            en_passant_square: None, // defined to be "the square the capturing pawn ends up on"
            halfmove_clock: 0, // moves without a pawn move or capture, for 50-move rule
            fullmove_number: 0,
        }
    }

    pub fn make_move(&mut self, move_to_make: Move) {
        let start_bitboard = 1u64 << move_to_make.start_square;
        let end_bitboard = 1u64 << move_to_make.end_square;

        let piece_moved = move_to_make.piece_moved;
        let colour_moved = self.side_to_move;

        let move_type = move_to_make.move_type;


        // remove piece at start
        self.bitboards.pieces[colour_moved as usize][piece_moved as usize] ^= start_bitboard;

        if let Some(captured) = move_to_make.captured {
            // remove captured piece
            self.bitboards.pieces[1 - (colour_moved as usize)][captured as usize] ^=
                end_bitboard;
        }

        if move_to_make.promotion.is_none() {
            // place piece
            self.bitboards.pieces[colour_moved as usize][piece_moved as usize] ^= end_bitboard;
        } else {
            // add promoted piece
            let promotion_piece = move_to_make.promotion.unwrap();
            self.bitboards.pieces[colour_moved as usize][promotion_piece as usize] ^=
                end_bitboard;
        }
        

        if [MoveType::CastleKingside, MoveType::CastleQueenside].contains(&move_type) {
            // implement the rook move
            let (rook_start, rook_end) = match (colour_moved, move_type) {
                (PieceColour::White, MoveType::CastleKingside) => (7, 5),
                (PieceColour::Black, MoveType::CastleKingside) => (63, 61),
                (PieceColour::White, MoveType::CastleQueenside) => (0, 3),
                (PieceColour::Black, MoveType::CastleQueenside) => (56, 59),
                _ => unreachable!("Non-castling move snuck into castling logic")
            };

            // remove rook from original square and add to new square
            let rook_move_bb = (1u64 << rook_start) | (1u64 << rook_end);
            self.bitboards.pieces[colour_moved as usize][PieceKind::Rook as usize] ^= rook_move_bb;
        };

        if move_type == MoveType::EnPassant {
            // square to remove pawn from is the location of the double push
            // so 1 rank behind en_passant_square if white captures
            let captured_pawn_square = self.en_passant_square.unwrap() << match colour_moved {
                PieceColour::White => -8,
                PieceColour::Black => 8,
            };
            // remove captured pawn from opposition bitboard
            self.bitboards.pieces[1 - (colour_moved as usize)][0] ^= captured_pawn_square;
        }
        self.bitboards.recompute_aggregates();
        self.update_internal_state_params( &move_to_make)

    }

    fn update_internal_state_params(&mut self, move_made: &Move) {
        // fullmove number increases by 1 regardless
        self.fullmove_number += 1;

        // if move not capture & not pawn move, add 1 to halfmove clock
        if move_made.captured.is_none() & !(move_made.piece_moved == PieceKind::Pawn) {
            self.halfmove_clock += 1;
        }

        // castling rights lost if rook moves, king moves, rook captured
        // TODO rewrite in a slicker way
        if move_made.piece_moved == PieceKind::King {
            match self.side_to_move {
                PieceColour::White => {
                    self.castling_rights.white_kingside = false;
                    self.castling_rights.white_queenside = false;
                },
                PieceColour::Black => {
                    self.castling_rights.black_kingside = false;
                    self.castling_rights.black_queenside = false;
                },
            }
        }

        if move_made.piece_moved == PieceKind::Rook {
            match move_made.start_square {
                0 => {self.castling_rights.white_queenside = false;},
                7 => {self.castling_rights.white_kingside = false;},
                56 => {self.castling_rights.black_queenside = false;},
                63 => {self.castling_rights.black_kingside = false;},
                _ => ()
            };
        }

        // not super slick but works
        if move_made.captured.unwrap_or(PieceKind::Pawn) == PieceKind::Rook {
            match move_made.end_square {
                0 => {self.castling_rights.white_queenside = false;},
                7 => {self.castling_rights.white_kingside = false;},
                56 => {self.castling_rights.black_queenside = false;},
                63 => {self.castling_rights.black_kingside = false;},
                _ => ()
            }
        }

        // en passant square
        if move_made.piece_moved == PieceKind::Pawn {
            let (start, end) = (move_made.start_square as i16, move_made.end_square as i16);
            // should only be true if pawn has moved two squares
            if (end - start) % 16 == 0  {
                // set en_passant_square to be the midpoint of the start and end moves
                self.en_passant_square = Some(1u64 << (start + (end - start) / 2))
            }
        }

        // pass turn over
        match self.side_to_move {
            PieceColour::White => {self.side_to_move = PieceColour::Black;},
            PieceColour::Black => {self.side_to_move = PieceColour::White;},
        }
    }

    pub fn print_to_screen(&self, font: &Font) {
        let line_spacing = 30.;

        for rank in 0..8 {
            let mut text_rank = String::with_capacity(9);
            for file in 0..8 {
                let square_index = 8 * rank + file;
                match self.bitboards.piece_at_square(square_index) {
                    Some(c) => text_rank.push(c.to_fen_char()),
                    None => text_rank.push('_'),
                }
            }

            draw_text_ex(
                &text_rank,
                8. * SQUARE_SIZE + 25. + X_OFFSET,
                (7 - rank) as f32 * line_spacing + Y_OFFSET,
                TextParams {
                    font: Some(font),
                    font_size: 30,
                    color: WHITE,
                    ..Default::default()
                },
            );
        }

        let state_info = [
            format!("Side to move: {:?}", self.side_to_move),
            format!(
                "Castling: {}{}{}{}",
                if self.castling_rights.white_kingside {
                    "K"
                } else {
                    "-"
                },
                if self.castling_rights.white_queenside {
                    "Q"
                } else {
                    "-"
                },
                if self.castling_rights.black_kingside {
                    "k"
                } else {
                    "-"
                },
                if self.castling_rights.black_queenside {
                    "q"
                } else {
                    "-"
                },
            ),
            format!(
                "En passant: {}",
                self.en_passant_square.is_some()
            ),
            format!("Halfmove clock: {}", self.halfmove_clock),
            format!("Fullmove number: {}", self.fullmove_number),
        ];

        for (i, info) in state_info.iter().enumerate() {
            draw_text_ex(
                info,
                8. * SQUARE_SIZE + 25. + X_OFFSET,
                8. * line_spacing + Y_OFFSET + (i as f32 + 1.) * 24.,
                TextParams {
                    font: Some(font),
                    font_size: 30,
                    color: WHITE,
                    ..Default::default()
                },
            );
        }
    }

    fn pseudolegal_moves(&self) -> Vec<Move> {
        self.pawn_moves(self.side_to_move)
    }

    fn legal_moves(&self) -> Vec<Move> {
        self.pseudolegal_moves()
    }

    fn pawn_moves(&self, piece_colour: PieceColour) -> Vec<Move> {
        let mut moves: Vec<Move> = Vec::new();

        let mut pawns = self.bitboards.pieces[piece_colour as usize][0];

        while let Some(start) = pop_lsb(&mut pawns) {

            // one square forward
            let one_step: i8 = match piece_colour {
                PieceColour::White => 8,
                PieceColour::Black => -8,
            };

            let end = start as i8 + one_step;
            if !(0..64).contains(&end) {
                continue;
            }
            if is_empty(self.bitboards.occupied, end as usize) {
                push_pawn_move(
                    &mut moves,
                    start,
                    end as usize,
                    is_promotion_rank(piece_colour, start),
                );
                // two squares behaviour within check to see if first square empty
                if (
                    (piece_colour == PieceColour::White) & ((8..16).contains(&start))
                ) | (
                    (piece_colour == PieceColour::Black) & ((48..56).contains(&start))
                ) {
                    let end = (end + one_step) as usize;
                    if is_empty(self.bitboards.occupied, end) {
                        push_pawn_move(
                            &mut moves,
                            start,
                            end,
                            false,
                        )
                    }
                }
            }
        }
        moves
    }

    fn is_in_check(&self) {
        // to do; use is_square_attacked on king positions
    }

    fn is_square_attacked(&self) {
        // to do
    }

    pub fn legal_moves_from(&self, start: usize) -> Vec<Move> {
        self.legal_moves()
            .into_iter()
            .filter(|m| m.start_square == start)
            .collect()
    }
}

#[derive(Default, Clone)]
pub struct BitBoards {
    pieces: [[u64; 6]; 2], // accessed like pieces[piece_colour][piece_kind]
    white_pieces: u64,
    black_pieces: u64,
    occupied: u64,
}

fn push_pawn_move(moves: &mut Vec<Move>, start: usize, end: usize, promotion_rank: bool) {
    if promotion_rank {
        for piece_kind in [
            PieceKind::Queen,
            PieceKind::Knight,
            PieceKind::Rook,
            PieceKind::Bishop,
        ] {
            moves.push(Move {
                start_square: start,
                end_square: end,
                piece_moved: PieceKind::Pawn,
                captured: None,
                promotion: Some(piece_kind),
                move_type: MoveType::Normal,
            })
        }
    } else {
        moves.push(Move {
            start_square: start,
            end_square: end,
            piece_moved: PieceKind::Pawn,
            captured: None,
            promotion: None,
            move_type: MoveType::Normal,
        })
    }
}

fn is_empty(occupied: u64, square_index: usize) -> bool {
    (occupied & (1u64 << square_index)) == 0
}

fn is_promotion_rank(colour: PieceColour, square_index: usize) -> bool {
    match colour {
        PieceColour::White => square_index >= 56,
        PieceColour::Black => square_index <= 7,
    }
}

pub fn print_bitboard(bb: u64) {
    for rank in (0..8).rev() {
        for file in 0..8 {
            let square_index = rank * 8 + file;
            let mask = 1u64 << square_index;
            if bb & mask != 0 {
                print!("1 ");
            } else {
                print!(". ");
            }
        }
        println!();
    }
    println!();
}

impl BitBoards {
    pub fn recompute_aggregates(&mut self) {
        self.white_pieces = 0;
        self.black_pieces = 0;

        for piece_type in 0..6 {
            self.white_pieces |= self.pieces[0][piece_type];
            self.black_pieces |= self.pieces[1][piece_type];
        }

        self.occupied = self.white_pieces | self.black_pieces;
    }

    fn from_fen(fen: &str) -> BitBoards {
        let mut bitboard = BitBoards::default();
        let mut square_index = 0;
        let fen_board = fen.split_whitespace().next().unwrap();

        for character in fen_board.chars() {
            match character {
                '/' => continue, // new rank indicator
                '1'..='8' => square_index += character.to_digit(10).unwrap(),
                piece => {
                    let square_bit = 1u64 << square_index; // create a 64 bit 000...001, left shift the 1 to square position
                    let piece = Piece::from_fen_char(piece).unwrap();

                    bitboard.pieces[piece.colour as usize][piece.kind as usize] |= square_bit;
                    square_index += 1;
                }
            }
        }
        bitboard.recompute_aggregates();
        bitboard
    }

    fn is_bitboard_square_set(bitboard: u64, square_index: usize) -> bool {
        bitboard >> square_index & 1 != 0
    }

    pub fn piece_at_square(&self, square_index: usize) -> Option<Piece> {
        let square_bit = 1u64 << square_index;

        if self.occupied & square_bit == 0 {
            return None;
        }

        let colour = if self.white_pieces & square_bit != 0 {
            PieceColour::White
        } else {
            PieceColour::Black
        };

        for kind_index in 0..6 {
            if self.pieces[colour as usize][kind_index] & square_bit != 0 {
                return Some(Piece {
                    kind: PieceKind::try_from(kind_index).unwrap(),
                    colour,
                });
            }
        }

        unreachable!(
            "Piece either exists or it doesn't - maybe bitboard aggregates are out of sync?"
        )
    }

    fn piece_kind_at_square(boards: [u64; 6], square_bit: u64) -> Option<PieceKind> {
        for (kind_index, &bitboard) in boards.iter().enumerate() {
            if bitboard & square_bit != 0 {
                return Some(PIECE_KINDS[kind_index]);
            }
        }
        None
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

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum MoveType {
    Normal,
    CastleKingside,
    CastleQueenside,
    EnPassant,
}

fn index_lsb(bitboard: u64) -> Option<usize> {
    if bitboard == 0 {
        None
    } else {
        Some(bitboard.trailing_zeros() as usize)
    }
}

fn pop_lsb(bitboard: &mut u64) -> Option<usize> {
    let square_index = index_lsb(*bitboard)?;
    *bitboard &= *bitboard - 1;
    Some(square_index)
}
