use crate::game::*;
use crate::attack_masks::masks::*;
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

// 0-3 are increasing along the ray; 4-7 decreasing
const ATTACK_MASKS: [[u64; 64]; 8] = [NORTHWEST_RAY, NORTH_RAY, NORTHEAST_RAY, EAST_RAY, SOUTHEAST_RAY, SOUTH_RAY, SOUTHWEST_RAY, WEST_RAY];
const BISHOP_DIRECTIONS: [usize; 4] = [0, 2, 4, 6];
const ROOK_DIRECTIONS: [usize; 4] = [1, 3, 5, 7];
const QUEEN_DIRECTIONS: [usize; 8] = [0, 1, 2, 3, 4, 5, 6, 7];

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
    fn new(piece_colour: PieceColour, piece_kind: PieceKind) -> Self {
        Piece {
            colour: piece_colour,
            kind: piece_kind
        }
    }

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
        Some(Piece::new(piece_colour, piece_kind))
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
// core methods
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

    pub fn legal_moves(&self) -> Vec<Move> {
        self.pseudolegal_moves()
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
            let promotion_piece = move_to_make.promotion.expect("Non-promotion move snuck into promotion logic");
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
            let captured_pawn_square = self.en_passant_square.expect("Non-en passant move snuck into EP logic") << match colour_moved {
                PieceColour::White => -8,
                PieceColour::Black => 8,
            };
            // remove captured pawn from opposition bitboard
            self.bitboards.pieces[1 - (colour_moved as usize)][0] ^= captured_pawn_square;
        }
        self.bitboards.recompute_aggregates();
        self.update_internal_state_params( &move_to_make)

    }

    pub fn legal_moves_from(&self, start: usize) -> Vec<Move> {
        self.legal_moves()
            .into_iter()
            .filter(|m| m.start_square == start)
            .collect()
    }
}

// movegen
impl GameState {
    fn pseudolegal_moves(&self) -> Vec<Move> {
        let mut psl_moves: Vec<Move> = Vec::new();
        psl_moves.extend(self.pawn_moves(self.side_to_move));
        psl_moves.extend(self.knight_moves(self.side_to_move));
        psl_moves.extend(self.bishop_moves(self.side_to_move));
        psl_moves.extend(self.rook_moves(self.side_to_move));
        psl_moves.extend(self.queen_moves(self.side_to_move));
        psl_moves.extend(self.king_moves(self.side_to_move));
        psl_moves
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

            let end = (start as i8 + one_step) as usize;
            if !(0..64).contains(&end) {
                continue;
            }
            if is_empty(self.bitboards.occupied, end) {
                if is_promotion_rank(piece_colour, end) {
                    moves.extend(Move::promotions(start, end, None))
                } else {
                    moves.push(Move::quiet(start, end, PieceKind::Pawn))
                }

                // two squares behaviour within check to see if first square empty
                if is_double_push_rank(piece_colour, start) {
                    let end = (end as i8 + one_step) as usize;
                    if is_empty(self.bitboards.occupied, end) {
                        moves.push(Move::quiet(start, end, PieceKind::Pawn));
                    }
                }
            }
            
            
            let mut pawn_attack_mask = match piece_colour {
                PieceColour::White => WHITE_PAWN_ATTACKS[start],
                PieceColour::Black => BLACK_PAWN_ATTACKS[start],
            };

            while let Some(end) = pop_lsb(&mut pawn_attack_mask) {
                if let Some(captured_piece) = self.bitboards.enemy_at_square(end, piece_colour) {
                    if is_promotion_rank(piece_colour, start) {
                        moves.extend(Move::promotions(start, end, Some(captured_piece)))
                    } else {
                        moves.push(Move::capture(start, end, PieceKind::Pawn, captured_piece))
                    }
                }
            }
        }
        // TODO enpassant
        moves
    }

    fn knight_moves(&self, piece_colour: PieceColour) -> Vec<Move> {
        let mut moves = Vec::new();
        let mut knights = self.bitboards.pieces[piece_colour as usize][1];

        while let Some(start) = pop_lsb(&mut knights) {
            // get attack board for square
            let mut knight_mask = KNIGHT_ATTACKS[start];
            // loop over possible target squares
            while let Some(end) = pop_lsb(&mut knight_mask) {
                if is_empty(self.bitboards.occupied, end) {
                    moves.push(Move::quiet(start, end, PieceKind::Knight));
                }
                // captures
                let enemies = match piece_colour {
                    PieceColour::White => self.bitboards.black_pieces,
                    PieceColour::Black => self.bitboards.white_pieces,
                };


                if is_capturable(enemies, end) {
                    let captured_piece = self.bitboards.enemy_at_square(end, piece_colour).expect("Uncapturable piece snuck into knight captures");
                    moves.push(Move::capture(start, end, PieceKind::Knight, captured_piece));
                }
            }

        }
        moves
    }

    fn bishop_moves(&self, piece_colour: PieceColour) -> Vec<Move> {
        let mut moves: Vec<Move> = vec![];
        let piece = Piece::new(piece_colour, PieceKind::Bishop);
        let mut bishops = self.bitboards.pieces[piece_colour as usize][2];

        while let Some(start) = pop_lsb(&mut bishops) {
            moves.extend(self.slider_moves(start, piece, &BISHOP_DIRECTIONS));
        }
        moves
    }

    fn rook_moves(&self, piece_colour: PieceColour) -> Vec<Move> {
        let mut moves: Vec<Move> = vec![];
        let piece = Piece::new(piece_colour, PieceKind::Rook);
        let mut rooks = self.bitboards.pieces[piece_colour as usize][3];

        while let Some(start) = pop_lsb(&mut rooks) {
            moves.extend(self.slider_moves(start, piece, &ROOK_DIRECTIONS));
        }
        moves
    }

    fn queen_moves(&self, piece_colour: PieceColour) -> Vec<Move> {
        let mut moves: Vec<Move> = vec![];
        let piece = Piece::new(piece_colour, PieceKind::Queen);
        let mut queens = self.bitboards.pieces[piece_colour as usize][4];

        while let Some(start) = pop_lsb(&mut queens) {
            moves.extend(self.slider_moves(start, piece, &QUEEN_DIRECTIONS));
        }
        moves
    }

    fn slider_moves(&self, start: usize, piece: Piece, directions: &[usize]) -> Vec<Move> {
        let mut moves: Vec<Move> = Vec::new();
        let occupied = self.bitboards.occupied;

        for &direction in directions {
            let mut ray = ATTACK_MASKS[direction][start];
            let blockers = ray & occupied;

            if blockers != 0 {
                let blocker_square = Self::closest_blocker(direction, blockers);

                ray &= Self::mask_up_to_exclusive(start, &direction, blocker_square);

                // if blocker is capturable, capture it
                if let Some(target_piece) = self.bitboards.piece_at_square(blocker_square) {
                    if target_piece.colour != piece.colour {
                        moves.push(Move::capture(start, blocker_square, piece.kind, target_piece.kind));
                    }
                }
                // otherwise, append quiet moves
                while let Some(end) = pop_lsb(&mut ray) {
                    moves.push(Move::quiet(start, end, piece.kind))
                }
            } else {
                while let Some(end) = pop_lsb(&mut ray) {
                    moves.push(Move::quiet(start, end, piece.kind))
                }
            }
        }
        moves
    }

    fn king_moves(&self, piece_colour: PieceColour) -> Vec<Move> {
        let mut moves: Vec<Move> = Vec::new();
        let mut king_bb = self.bitboards.pieces[piece_colour as usize][5];
        let start = pop_lsb(&mut king_bb).expect("No king of colour {piece_colour} left on the board");
        let mut attack_mask = KING_ATTACKS[start];

        while let Some(end) = pop_lsb(&mut attack_mask) {
            if is_empty(self.bitboards.occupied, end) {
                moves.push(Move {
                    start_square: start,
                    end_square: end,
                    piece_moved: PieceKind::King,
                    captured: None,
                    promotion: None,
                    move_type: MoveType::Normal,
                })
            };

            let enemies = match piece_colour {
                PieceColour::White => self.bitboards.black_pieces,
                PieceColour::Black => self.bitboards.white_pieces,
            };

            if is_capturable(enemies, end) {
                let enemy = self.bitboards.enemy_at_square(end, piece_colour);

                moves.push({Move {
                    start_square: start,
                    end_square: end,
                    piece_moved: PieceKind::King,
                    captured: enemy,
                    promotion: None,
                    move_type: MoveType::Normal,
                }})
                }
        };
        moves
    }

}

// misc helpers
impl GameState {

    fn closest_blocker(direction: usize, blockers: u64) -> usize {
        match direction {
            0..4 => index_lsb(blockers).expect("No LSB found"),
            4..8 => index_msb(blockers).expect("No MSB found"),
            _ => unreachable!("direction index overflow")
        }
    }

    fn mask_up_to_exclusive(start: usize, direction: &usize, blocker_square: usize) -> u64 {
        let step = match direction {
            0 => 7,
            1 => 8,
            2 => 9,
            3 => 1,
            4 => -7,
            5 => -8,
            6 => -9,
            7 => -1,
            _ => unreachable!()
        };

        let mut mask = 0u64;
        let mut square = start as isize;

        loop {
            square += step;
            if (0..64).contains(&square) | (square == blocker_square as isize) { break };
            mask |= 1u64 << square;
        }
        mask 
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
}
#[derive(Default, Clone)]
pub struct BitBoards {
    pub pieces: [[u64; 6]; 2], // accessed like pieces[piece_colour][piece_kind]
    pub white_pieces: u64,
    pub black_pieces: u64,
    occupied: u64,
}

fn is_empty(occupied: u64, square_index: usize) -> bool {
    (occupied & (1u64 << square_index)) == 0
}

fn is_capturable(enemies: u64, square_index: usize) -> bool {
    (enemies & (1u64 << square_index)) != 0
}

fn is_promotion_rank(colour: PieceColour, square_index: usize) -> bool {
    match colour {
        PieceColour::White => square_index >= 56,
        PieceColour::Black => square_index <= 7,
    }
}

fn is_double_push_rank(piece_colour: PieceColour, start: usize) -> bool {
    (piece_colour == PieceColour::White) & ((8..16).contains(&start)) | 
    (piece_colour == PieceColour::Black) & ((48..56).contains(&start))
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
                return Some(Piece::new(
                    colour,
                    PieceKind::try_from(kind_index).unwrap(),
                ));
            }
        }

        unreachable!(
            "Piece either exists or it doesn't - maybe bitboard aggregates are out of sync?"
        )
    }

    fn enemy_at_square(&self, square: usize, piece_colour: PieceColour) -> Option<PieceKind> {
        let enemy_bitboards = self.pieces[1 - (piece_colour as usize)];
        for (index, bitboard) in enemy_bitboards.iter().enumerate() {
            if bitboard & (1u64 << square) != 0 {
                return Some(PieceKind::try_from(index).expect("PieceKind index out of bounds"))
            }
        };
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
impl Move {
    fn quiet(start: usize, end: usize, piece_kind: PieceKind) -> Move {
        Move {
            start_square: start,
            end_square: end,
            piece_moved: piece_kind,
            captured: None,
            promotion: None,
            move_type: MoveType::Normal,
        }
    }

    fn capture(start: usize, end: usize, piece_kind: PieceKind, target_kind: PieceKind) -> Move {
        Move {
            start_square: start,
            end_square: end,
            piece_moved: piece_kind,
            captured: Some(target_kind),
            promotion: None,
            move_type: MoveType::Normal,
        }
    }

    fn promotions(start: usize, end: usize, target_kind: Option<PieceKind>) -> Vec<Move> {
        let promotions = [PieceKind::Knight, PieceKind::Bishop, PieceKind::Rook, PieceKind::Queen];
        promotions.into_iter()
            .map(|promotion| Move {
                start_square: start,
                end_square: end,
                piece_moved: PieceKind::Pawn,
                captured: target_kind,
                promotion: Some(promotion),
                move_type: MoveType::Normal,
            })
            .collect()
    }


}



#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum MoveType {
    Normal,
    CastleKingside,
    CastleQueenside,
    EnPassant,
}

// bit methods
fn index_lsb(bitboard: u64) -> Option<usize> {
    if bitboard == 0 {
        None
    } else {
        Some(bitboard.trailing_zeros() as usize)
    }
}

fn index_msb(bitboard: u64) -> Option<usize> {
    if bitboard == 0 {
        None
    } else {
        Some(63 - bitboard.leading_zeros() as usize)
    }
}

fn pop_lsb(bitboard: &mut u64) -> Option<usize> {
    let lsb_idx = index_lsb(*bitboard)?;
    *bitboard &= *bitboard - 1;
    Some(lsb_idx)
}
