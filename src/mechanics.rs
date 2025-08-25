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

/// **Encodes piece types with a uzise discriminant**
///
/// *Methods* - [from_fen_char]
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum PieceKind {
    Pawn = 0,
    Knight = 1,
    Bishop = 2,
    Rook = 3,
    Queen = 4,
    King = 5,
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

/// ---
/// **Efficient representation of piece locations**
///
/// array indices - [white, black] | [pawns, knights, bishops, rooks, queens, king]
///
/// *Methods* - [find_friendly_pieces], [set_piece], [from_fen], [is_bitboard_square_set], [piece_at_square]
#[derive(Default)]
pub struct BitBoards {
    pieces: [[u64; 6]; 2],
}

impl BitBoards {
    fn find_friendly_pieces(&self, colour: PieceColour) -> u64 {
        self.pieces[colour as usize]
            .iter()
            .copied()
            .reduce(|a, b| a | b)
            .unwrap_or(0u64)
    }

    pub fn from_fen(fen: &str) -> BitBoards {
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
        bitboard
    }

    pub fn print_bitboards_to_screen(&self, font: &Font) {
        let line_spacing = 30;

        for rank in (0..8).rev() {
            let mut text_rank = String::with_capacity(9);

            for file in 0..8 {
                let square_index = 8 * rank + file;
                match self.piece_at_square(square_index) {
                    Some(c) => text_rank.push(c.to_fen_char()),
                    None => text_rank.push('_'),
                }
            }

            draw_text_ex(
                &text_rank,
                8. * SQUARE_SIZE + 25. + X_OFFSET,
                (rank * line_spacing) as f32 + Y_OFFSET,
                TextParams {
                    font: Some(font),
                    font_size: 30,
                    color: WHITE,
                    ..Default::default()
                }
            );        
        }
    }

    fn is_bitboard_square_set(bitboard: u64, square_index: usize) -> bool {
        bitboard >> square_index & 1 != 0
    }

    pub fn piece_at_square(&self, square_index: usize) -> Option<Piece> {
        let square_bit = 1u64 << square_index;

        for (colour_idx, &colour) in PIECE_COLOURS.iter().enumerate() {
            if let Some(kind) = Self::piece_kind_at_square(self.pieces[colour_idx], square_bit) {
                return Some(Piece { kind, colour });
            }
        }
        None
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
pub struct Move {
    start_square: usize,
    end_square: usize,
    promotion: Option<Piece>,
}
