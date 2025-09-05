use crate::game_state::*;
use crate::attack_masks::masks::*;
use crate::constants::*;
use macroquad::prelude::*;
use arrayvec::ArrayString;

pub type BitBoard = u64;

#[derive(Default, Clone)]
pub struct BitBoards {
    pub pieces: [[BitBoard; 6]; 2], // pieces[piece_colour][piece_kind]
    pub white_pieces: BitBoard,
    pub black_pieces: BitBoard,
    pub occupied: BitBoard,
    pub attacked_by_white: BitBoard,
    pub attacked_by_black: BitBoard,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Piece {
    pub kind: PieceKind,
    pub colour: PieceColour,
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
impl CastlingRights {
    pub fn to_u8(&self) -> u8 {
        (self.white_kingside as u8) << 0 |
        (self.white_queenside as u8) << 1 |
        (self.black_kingside as u8) << 2 |
        (self.black_queenside as u8) << 3
    }
}

#[derive(Clone, Debug)]
pub struct PinsAndCheckers {
    pub pins: [BitBoard; 64], // pinned piece locations indexing within-pin move masks; !0 for no pin
    pub check_mask: BitBoard // optimisation. not in check -> !0, single check is a nontrivial ray, double check -> 0
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
        // side to move should be the side with the king which may be in check / pieces which may be pinned
        let pinning_side = 1 - side_to_move as usize;
        let mut pin_array = [!0; 64];
        let mut check_mask = !0; // initialise with no checks

        let king_bb = self.pieces[side_to_move as usize][5];
        assert!(
            king_bb.count_ones() == 1,
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
                    let opposite_direction = (4 + direction) % 8;

                    let ray_up_to_king =
                        mask_up_to_inclusive(king_square, &opposite_direction, start);

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
        
        if check_mask != 0 { // if it's already double check, we won't find any others
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
        
        PinsAndCheckers { pins: pin_array, check_mask }
    }
}

// attack mask generation
impl BitBoards {
    fn pawn_attacks(&self, piece_colour: PieceColour) -> BitBoard {
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

    fn knight_attacks(&self, piece_colour: PieceColour) -> BitBoard {
        let mut current_knight_attack_masks = 0u64;
        let mut knights = self.pieces[piece_colour as usize][1];

        while let Some(start) = pop_lsb(&mut knights) {
            current_knight_attack_masks |= KNIGHT_ATTACKS[start];
        }
        current_knight_attack_masks
    }

    fn king_attacks(&self, piece_colour: PieceColour) -> BitBoard {
        let king_bb = self.pieces[piece_colour as usize][5];

        assert!(
            king_bb.count_ones() == 1,
            "King bitboard misfire for {piece_colour:?}"
        );
        let king_position = self.pieces[piece_colour as usize][5].trailing_zeros() as usize;

        KING_ATTACKS[king_position]
    }

    fn directional_attacks(&self, slider_locations: BitBoard, slider_colour: usize, direction: usize) -> BitBoard {
        let mut current_slider_attack_masks = 0u64;
        let mut sliders = slider_locations;
        let occupied = self.occupied;
        let attacked_colour = 1 - slider_colour;

        while let Some(start) = pop_lsb(&mut sliders) {
            let ray = ATTACK_MASKS[direction][start];
            let blockers = (ray & occupied) & !self.pieces[attacked_colour][5];
            // king doesn't count as a blocker
            if blockers != 0 {
                // squares up to and including blocker are 'attacked', regardless of blocker colour (ie defended,  if blocker is same colour)
                let blocker_square = closest_blocker(direction, blockers);
                let ray_up_to_blocker =
                    ray & mask_up_to_inclusive(start, &direction, blocker_square);
                current_slider_attack_masks |= ray_up_to_blocker;
            } else {
                current_slider_attack_masks |= ray;
            }
        }
        current_slider_attack_masks
    }

    fn slider_attacks(&self, piece_kind: usize, piece_colour: PieceColour) -> BitBoard {
        let mut current_slider_attack_masks = 0u64;
        let sliders = self.pieces[piece_colour as usize][piece_kind];
        let directions = match piece_kind {
            2 => BISHOP_DIRECTIONS,
            3 => ROOK_DIRECTIONS,
            4 => QUEEN_DIRECTIONS,
            _ => unreachable!(
                "trying to calculate slider attack mask behaviour for non-slider piece"
            ),
        };

        for &direction in directions {
            current_slider_attack_masks |= self.directional_attacks(sliders, piece_colour as usize, direction);
        }
        current_slider_attack_masks
    }

    fn all_attacked_squares(&self, piece_colour: PieceColour) -> BitBoard {
        let mut attacked_squares = 0u64;
        attacked_squares |= self.pawn_attacks(piece_colour);
        attacked_squares |= self.knight_attacks(piece_colour);
        attacked_squares |= self.slider_attacks(PieceKind::Bishop as usize, piece_colour);
        attacked_squares |= self.slider_attacks(PieceKind::Rook as usize, piece_colour);
        attacked_squares |= self.slider_attacks(PieceKind::Queen as usize, piece_colour);
        attacked_squares |= self.king_attacks(piece_colour);
        attacked_squares
    }
}

// helpers
impl BitBoards {

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

        assert!(
            opposing_king_bb.count_ones() == 1,
            "Incorrect number of kings for {pinning_side:?}"
        );

        // ray doesn't count if not pointing at the king
        if opposing_king_bb & ray == 0 {
            return false;
        };

        let pin_side_pieces = self.get_coloured_pieces(pinning_side); 

        // if ray blocked by opposing pieces, no pins/checks to consider
        let ray_excluding_king = ray & !opposing_king_bb;
        let ray_excluding_start = ray_excluding_king & !(1u64 << start);

        if ray_excluding_start & pin_side_pieces != 0 {
            return false;
        }

        true
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
                return Some(Piece::new(colour, PieceKind::try_from(kind_index).unwrap()));
            }
        }

        unreachable!(
            "Piece either exists or it doesn't - maybe bitboard aggregates are out of sync?"
        )
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

    pub fn draw_attack_masks_to_screen(&self, font: &Font) {
        let white_strings = bb_to_screen_printable_string(&self.attacked_by_white);
        let black_strings = bb_to_screen_printable_string(&self.attacked_by_black);

        draw_text_ex(
            "White attack masks",
            100.,
            150.,
            TextParams {
                font: Some(font),
                font_size: 20,
                color: WHITE,
                ..Default::default()
            },
        );

        draw_text_ex(
            "Black attack masks",
            100.,
            600.,
            TextParams {
                font: Some(font),
                font_size: 20,
                color: WHITE,
                ..Default::default()
            },
        );

        for (rank, line) in white_strings.iter().enumerate() {
            for (file, ch) in line.chars().enumerate() {
                draw_text_ex(
                    &ch.to_string(),
                    100. + file as f32 * 20., // X offset for columns
                    200. + rank as f32 * 35., // Y offset for rows
                    TextParams {
                        font: Some(font),
                        font_size: 20,
                        color: WHITE,
                        ..Default::default()
                    },
                );
            }
        }

        for (rank, line) in black_strings.iter().enumerate() {
            for (file, ch) in line.chars().enumerate() {
                draw_text_ex(
                    &ch.to_string(),
                    100. + file as f32 * 20., // X offset for columns
                    650. + rank as f32 * 35., // Y offset for rows
                    TextParams {
                        font: Some(font),
                        font_size: 20,
                        color: WHITE,
                        ..Default::default()
                    },
                );
            }
        }
    }

    pub fn get_coloured_pieces(&self, piece_colour: usize) -> BitBoard {
        match piece_colour {
            0 => self.white_pieces,
            1 => self.black_pieces,
            _ => unreachable!("can't look up coloured pieces for non-binary digit"),
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

impl Piece {
    pub fn new(piece_colour: PieceColour, piece_kind: PieceKind) -> Self {
        Piece {
            colour: piece_colour,
            kind: piece_kind,
        }
    }

    pub fn from_fen_char(c: char) -> Option<Piece> {
        let piece_colour = if c.is_uppercase() {
            PieceColour::White
        } else {
            PieceColour::Black
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

impl CastlingRights {
    pub fn check(&self, piece_colour: PieceColour, side: MoveType) -> bool {
        match (side, piece_colour) {
            (MoveType::CastleQueenside, PieceColour::White) => self.white_queenside,
            (MoveType::CastleQueenside, PieceColour::Black) => self.black_queenside,
            (MoveType::CastleKingside, PieceColour::White) => self.white_kingside,
            (MoveType::CastleKingside, PieceColour::Black) => self.black_kingside,
            _ => unreachable!("you need a castling movetype")
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

pub fn mask_up_to_exclusive(start: usize, direction: &usize, blocker_square: usize) -> BitBoard {
    let step = match direction {
        0 => 7,  // northwest
        1 => 8,  // north
        2 => 9,  // northeast
        3 => 1,  // east
        4 => -7, // southeast
        5 => -8, // south
        6 => -9, // southwest
        7 => -1, // west
        _ => unreachable!(),
    };

    let mut mask = 0u64;
    let mut square = start as isize;
    let mut previous_file = start % 8; // keep track of which file we were before stepping

    loop {
        square += step;
        if !(0..64).contains(&square) {
            break;
        } // gone off end of board
        let new_file = (square as usize) % 8; // which file are we on after stepping
        let file_diff = (new_file as isize - previous_file as isize).abs();
        if file_diff > 1 {
            break;
        }; // if file has changed by more than one, we must have wrapped around
        previous_file = new_file;

        if square == blocker_square as isize {
            break;
        }; // break out before appending final square
        mask |= 1u64 << square;
    }
    mask
}

pub fn mask_up_to_inclusive(start: usize, direction: &usize, blocker_square: usize) -> BitBoard {
    let step = match direction {
        0 => 7,  // northwest
        1 => 8,  // north
        2 => 9,  // northeast
        3 => 1,  // east
        4 => -7, // southeast
        5 => -8, // south
        6 => -9, // southwest
        7 => -1, // west
        _ => unreachable!(),
    };

    let mut mask = 0u64;
    let mut square = start as isize;
    let mut previous_file = start % 8; // keep track of which file we were before stepping

    loop {
        square += step;
        if !(0..64).contains(&square) {
            break;
        } // gone off end of board
        let new_file = (square as usize) % 8; // which file are we on after stepping
        let file_diff = (new_file as isize - previous_file as isize).abs();
        if file_diff > 1 {
            break;
        }; // if file has changed by more than one, we must have wrapped around
        previous_file = new_file;

        mask |= 1u64 << square;
        if square == blocker_square as isize {
            break;
        };
    }
    mask
}

// bit methods
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

#[allow(dead_code)]
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