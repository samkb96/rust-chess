use crate::constants::magic::lookup_magic;
use crate::constants::masks::*;
use crate::constants::misc::*;
use crate::mechanics::PieceKind::Bishop;
use crate::mechanics::PieceKind::Queen;
use crate::mechanics::PieceKind::Rook;
use arrayvec::ArrayString;
use macroquad::prelude::*;

pub type BitBoard = u64;

#[derive(Default, Clone)]
pub struct BitBoards {
    pub pieces: [[BitBoard; 6]; 2], // pieces[piece_colour][piece_kind]
    pub white_pieces: BitBoard,
    pub black_pieces: BitBoard,
    pub occupied: BitBoard,
    pub attacked_by: [[BitBoard; 6]; 2],
    pub attacked_by_white: BitBoard,
    pub attacked_by_black: BitBoard,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Piece {
    pub kind: PieceKind,
    pub colour: PieceColour,
    pub id: u8,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum PieceColour {
    White = 0,
    Black = 1,
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

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct CastlingRights {
    pub white_kingside: bool,
    pub white_queenside: bool,
    pub black_kingside: bool,
    pub black_queenside: bool,
}

#[derive(Clone, Debug)]
pub struct PinsAndCheckers {
    // pinned piece locations indexing within-pin move masks; !0 for no pin
    pub pins: [BitBoard; 64],
    // optimisation. not in check -> !0, single check is a nontrivial ray, double check -> 0
    pub check_mask: BitBoard,
}

// pins, checks, aggregates
impl BitBoards {
    pub fn recompute_aggregates(&mut self) {
        self.white_pieces = 0;
        self.black_pieces = 0;

        for piece_type in 0..6 {
            self.white_pieces |= self.pieces[0][piece_type];
            self.black_pieces |= self.pieces[1][piece_type];
        }

        self.occupied = self.white_pieces | self.black_pieces;

        self.attacked_by_white = self.all_attacked_squares(PieceColour::White);
        self.attacked_by_black = self.all_attacked_squares(PieceColour::Black);
    }

    pub fn get_pins_and_checks(&self, side_to_move: PieceColour) -> PinsAndCheckers {
        // TODO unravel crazy nesting. Very difficult to see what's going on

        // side to move should be the side with the king which may be in check / pieces which may be pinned

        let pinning_side = 1 - side_to_move as usize;
        let mut pin_array = [!0; 64];
        let mut check_mask = !0; // initialise with no checks

        let king_bb = self.pieces[side_to_move as usize][5];
        assert_eq!(
            king_bb.count_ones(),
            1,
            "Wrong number of kings on {side_to_move:?} bitboards"
        );
        let king_square = king_bb.trailing_zeros() as usize;

        // sliders
        for (immutable_sliders, slider_directions) in [
            (self.pieces[pinning_side][2], BISHOP_DIRECTIONS),
            (self.pieces[pinning_side][3], ROOK_DIRECTIONS),
            (self.pieces[pinning_side][4], QUEEN_DIRECTIONS),
        ] {
            let mut opposing_sliders = immutable_sliders;

            'slider_loop: while let Some(start) = pop_lsb(&mut opposing_sliders) {
                'direction_loop: for &direction in slider_directions {
                    let ray_mask_from_opposing_slider = ATTACK_MASKS[direction][start];

                    if !self.opposing_king_on_unobstructed_ray(
                        pinning_side,
                        start,
                        ray_mask_from_opposing_slider,
                    ) {
                        continue 'direction_loop; // try next direction
                    }

                    // include start in possible squares to move to (ie can capture pinner)

                    let ray_up_to_king = MASK_UP_TO_INCLUSIVE[king_square][start];

                    // count the number of pieces in the way to determine check/pinned/ok
                    match self.friendly_pieces_on_ray(side_to_move, ray_up_to_king) {
                        0 => {
                            check_mask &= ray_up_to_king; // sets correct mask if first check detected, and if two detected, no overlap => 0
                            continue 'slider_loop; // don't bother with other directions, as they won't be king-facing
                        }
                        1 => {
                            let pinned_piece_square =
                                (ray_up_to_king & self.get_coloured_pieces(side_to_move as usize))
                                    .trailing_zeros() as usize;
                            pin_array[pinned_piece_square] = ray_up_to_king;

                            continue 'slider_loop;
                        }
                        _ => (), // more than one friendly piece in the way; no check or pin
                    }
                }
            }
        }

        if check_mask != 0 {
            // if it's already double check, we won't find any others
            // knights
            let mut opposing_knights = self.pieces[pinning_side][1];

            while let Some(start) = pop_lsb(&mut opposing_knights) {
                let attacked_by_knight = KNIGHT_ATTACKS[start];
                if attacked_by_knight & king_bb != 0 {
                    check_mask &= 1u64 << start;
                    if check_mask == 0 {
                        break;
                    }
                }
            }

            // pawns
            let mut opposing_pawns = self.pieces[pinning_side][0];

            while let Some(start) = pop_lsb(&mut opposing_pawns) {
                let attacked_by_pawn = PAWN_ATTACKS[pinning_side][start];
                if attacked_by_pawn & king_bb != 0 {
                    check_mask &= 1u64 << start;

                    if check_mask == 0 {
                        break;
                    }
                }
            }
        }

        PinsAndCheckers {
            pins: pin_array,
            check_mask,
        }
    }
}

impl BitBoards {
    pub fn pawn_attacks(&self, piece_colour: PieceColour) -> BitBoard {
        let mut current_pawn_attack_masks = 0u64;
        let mut pawns = self.pieces[piece_colour as usize][0];
        let individual_attack_masks = match piece_colour {
            PieceColour::White => WHITE_PAWN_ATTACKS,
            PieceColour::Black => BLACK_PAWN_ATTACKS,
        };

        while let Some(start) = pop_lsb(&mut pawns) {
            current_pawn_attack_masks |= individual_attack_masks[start];
        }
        current_pawn_attack_masks
    }

    pub fn knight_attacks(&self, piece_colour: PieceColour) -> BitBoard {
        let mut current_knight_attack_masks = 0u64;
        let mut knights = self.pieces[piece_colour as usize][1];

        while let Some(start) = pop_lsb(&mut knights) {
            current_knight_attack_masks |= KNIGHT_ATTACKS[start];
        }
        current_knight_attack_masks
    }

    pub fn king_attacks(&self, piece_colour: PieceColour) -> BitBoard {
        let king_position = self.pieces[piece_colour as usize][5].trailing_zeros() as usize;

        KING_ATTACKS[king_position]
    }

    pub fn slider_attacks(&self, piece_kind: PieceKind, piece_colour: PieceColour) -> BitBoard {
        let mut sliders = self.get(piece_colour, piece_kind);
        let blockers = self.occupied;
        let trimmed_masks = match piece_kind {
            Bishop => TRIMMED_BISHOP_MASKS,
            Rook => TRIMMED_ROOK_MASKS,
            Queen => TRIMMED_QUEEN_MASKS,
            _ => unreachable!(),
        };

        let mut current_slider_attack_masks = 0u64;

        while let Some(slider_index) = pop_lsb(&mut sliders) {
            let trimmed_mask = trimmed_masks[slider_index];
            let blocker_configuration = trimmed_mask & blockers;

            let attack_mask = lookup_magic(piece_kind as u8, slider_index, blocker_configuration);
            current_slider_attack_masks |= attack_mask;
        }
        current_slider_attack_masks
    }

    pub fn all_attacked_squares(&mut self, piece_colour: PieceColour) -> BitBoard {
        let colour = piece_colour as usize;

        self.attacked_by[colour][0] = self.pawn_attacks(piece_colour);
        self.attacked_by[colour][1] = self.knight_attacks(piece_colour);
        self.attacked_by[colour][2] = self.slider_attacks(Bishop, piece_colour);
        self.attacked_by[colour][3] = self.slider_attacks(Rook, piece_colour);
        self.attacked_by[colour][4] = self.slider_attacks(Queen, piece_colour);
        self.attacked_by[colour][5] = self.king_attacks(piece_colour);

        let mut attacked_squares = 0u64;
        for i in 0usize..=5 {
            attacked_squares |= self.attacked_by[colour][i]
        }

        attacked_squares
    }
}

// helpers
impl BitBoards {
    pub fn get(&self, piece_colour: PieceColour, piece_kind: PieceKind) -> BitBoard {
        let u_colour = piece_colour as usize;
        let u_kind = piece_kind as usize;
        self.pieces[u_colour][u_kind]
    }

    fn friendly_pieces_on_ray(&self, piece_colour: PieceColour, ray: BitBoard) -> u32 {
        (self.get_coloured_pieces(piece_colour as usize) & ray).count_ones()
    }

    fn opposing_king_on_unobstructed_ray(
        &self,
        pinning_side: usize,
        start: usize,
        ray: BitBoard,
    ) -> bool {
        let opposing_king_bb = self.pieces[1 - pinning_side][5];
        assert_eq!(
            opposing_king_bb.count_ones(),
            1,
            "Incorrect number of kings for {pinning_side:?}"
        );

        // ray doesn't count if not pointing at the king
        if opposing_king_bb & ray == 0 {
            return false;
        };

        let pin_side_pieces = self.get_coloured_pieces(pinning_side);
        let king_square = opposing_king_bb.trailing_zeros();

        // if ray blocked by opposing pieces, no pins/checks to consider

        let ray_up_to_king = MASK_UP_TO_EXCLUSIVE[start][king_square as usize];

        if ray_up_to_king & pin_side_pieces != 0 {
            return false;
        }

        true
    }

    pub fn enemy_at_square(&self, square: usize, piece_colour: PieceColour) -> Option<PieceKind> {
        let enemy_bitboards = self.pieces[1 - (piece_colour as usize)];
        for (index, bitboard) in enemy_bitboards.iter().enumerate() {
            if bitboard & (1u64 << square) != 0 {
                return Some(PieceKind::try_from(index).expect("PieceKind index out of bounds"));
            }
        }
        None
    }

    pub fn get_coloured_pieces(&self, piece_colour: usize) -> BitBoard {
        match piece_colour {
            0 => self.white_pieces,
            1 => self.black_pieces,
            _ => panic!("can't look up coloured pieces for non-binary digit"),
        }
    }
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

impl TryFrom<u8> for PieceColour {
    type Error = ();
    fn try_from(colour_index: u8) -> Result<Self, Self::Error> {
        match colour_index {
            0 => Ok(PieceColour::White),
            1 => Ok(PieceColour::Black),
            _ => Err(()),
        }
    }
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

impl TryFrom<u8> for PieceKind {
    type Error = ();
    fn try_from(kind_index: u8) -> Result<Self, Self::Error> {
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

impl PieceColour {
    pub fn flip(self) -> Self {
        if self == PieceColour::White {
            PieceColour::Black
        } else {
            PieceColour::White
        }
    }
}

impl Piece {
    pub fn new(colour: PieceColour, kind: PieceKind, id: u8) -> Self {
        Piece { colour, kind, id }
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

impl CastlingRights {
    pub fn check(&self, piece_colour: u8, side: u8) -> bool {
        match (side, piece_colour) {
            (2, 0) => self.white_kingside,
            (2, 1) => self.black_kingside,
            (3, 0) => self.white_queenside,
            (3, 1) => self.black_queenside,
            _ => unreachable!("you need a castling movetype"),
        }
    }
}

// misc helpers
pub fn closest_blocker(direction: usize, blockers: BitBoard) -> usize {
    match direction {
        0..4 => index_lsb(blockers).expect("No LSB found"),
        4..8 => index_msb(blockers).expect("No MSB found"),
        _ => unreachable!("direction index overflow"),
    }
}

pub fn index_lsb(bitboard: BitBoard) -> Option<usize> {
    if bitboard == 0 {
        None
    } else {
        Some(bitboard.trailing_zeros() as usize)
    }
}

pub fn index_msb(bitboard: BitBoard) -> Option<usize> {
    if bitboard == 0 {
        None
    } else {
        Some(63 - bitboard.leading_zeros() as usize)
    }
}

pub fn pop_lsb(bitboard: &mut BitBoard) -> Option<usize> {
    let lsb_idx = index_lsb(*bitboard)?;
    *bitboard &= *bitboard - 1;
    Some(lsb_idx)
}

// bitboard debugging
pub fn bb_to_screen_printable_string(bb: &BitBoard) -> [String; 8] {
    let mut result: [String; 8] = Default::default();
    for (rank_idx, rank) in (0..8).enumerate() {
        // unecessary loop variable index warning fix
        let mut rank_str = String::with_capacity(8);
        for file in 0..8 {
            let square_index = (7 - rank) * 8 + file;
            let bit = bb >> square_index & 1;
            rank_str.push_str(if bit == 1 { "1 " } else { "_ " })
        }
        result[rank_idx] = rank_str;
    }
    result
}

pub fn print_bitboard(bb: BitBoard) {
    for rank in (0..8).rev() {
        print!("{}  ", rank + 1);
        for file in 0..8 {
            let square = rank * 8 + file;
            let mask = 1u64 << square;
            if bb & mask != 0 {
                print!(" 1");
            } else {
                print!(" .");
            }
        }
        println!();
    }
    println!("\n    a b c d e f g h\n");
}
