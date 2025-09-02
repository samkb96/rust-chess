use crate::attack_masks::masks::*;
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

// 0-3 are increasing along the ray; 4-7 decreasing
const ATTACK_MASKS: [[u64; 64]; 8] = [
    NORTHWEST_RAY,
    NORTH_RAY,
    NORTHEAST_RAY,
    EAST_RAY,
    SOUTHEAST_RAY,
    SOUTH_RAY,
    SOUTHWEST_RAY,
    WEST_RAY,
];
const BISHOP_DIRECTIONS: &[usize] = &[0, 2, 4, 6];
const ROOK_DIRECTIONS: &[usize] = &[1, 3, 5, 7];
const QUEEN_DIRECTIONS: &[usize] = &[0, 1, 2, 3, 4, 5, 6, 7];

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
            kind: piece_kind,
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

#[derive(Copy, Clone, PartialEq, Eq)]
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
    pins_and_checkers: PinsAndCheckers,
    side_to_move: PieceColour,
    castling_rights: CastlingRights,
    en_passant_square: Option<u64>,
    halfmove_clock: u16,
    fullmove_number: u16,
    previous_state: StateCache,
}

#[derive(Clone, PartialEq, Eq)]
struct PreviousState {
    last_move: Move,
    castling_rights: CastlingRights,
    en_passant_square: Option<u64>,
    halfmove_clock: u16,
}

type StateCache = Vec<PreviousState>;
// core methods
impl GameState {
    pub fn initialise() -> Self {
        GameState {
            bitboards: BitBoards::from_fen(INITIALISATION_FEN),
            pins_and_checkers: PinsAndCheckers {
                pins: vec![],
                checkers: vec![],
            },
            side_to_move: PieceColour::White,
            castling_rights: CastlingRights::initialise(),
            en_passant_square: None, // defined to be "the square the pawn just skipped over"
            halfmove_clock: 0,       // moves without a pawn move or capture, for 50-move rule
            fullmove_number: 0,
            previous_state: vec![],
        }
    }

    pub fn draw_pins_and_checks(&self, font: &Font) {
        let mut y_offset = 150.0; // starting y
        let x = 1300.0; // fixed x

        // Draw header
        draw_text_ex(
            "Pins:",
            x,
            y_offset,
            TextParams {
                font: Some(font),
                font_size: 20,
                color: WHITE,
                ..Default::default()
            },
        );
        y_offset += 25.0;

        // Draw each pinned piece
        for pin in &self.pins_and_checkers.pins {
            let text = format!("{} ", usize_to_square_name(pin.pinned_piece_square));
            draw_text_ex(
                &text,
                x,
                y_offset,
                TextParams {
                    font: Some(font),
                    font_size: 20,
                    color: WHITE,
                    ..Default::default()
                },
            );
            y_offset += 25.0;
        }

        y_offset += 10.0; // small gap before checkers
        draw_text_ex(
            "Checkers:",
            x,
            y_offset,
            TextParams {
                font: Some(font),
                font_size: 20,
                color: WHITE,
                ..Default::default()
            },
        );
        y_offset += 25.0;

        for checker in &self.pins_and_checkers.checkers {
            let text = usize_to_square_name(checker.checker_piece_square);
            draw_text_ex(
                &text,
                x,
                y_offset,
                TextParams {
                    font: Some(font),
                    font_size: 20,
                    color: WHITE,
                    ..Default::default()
                },
            );
            y_offset += 25.0;
        }
    }

    pub fn legal_moves(&self) -> Moves {
        self.pseudolegal_moves()
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
            self.bitboards.pieces[side_to_move][0] ^=
                previous_state.en_passant_square.unwrap();
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
        self.fullmove_number -= 1;

        // reset cached state
        self.castling_rights = previous_state.castling_rights;
        self.halfmove_clock = previous_state.halfmove_clock;
        self.en_passant_square = previous_state.en_passant_square;

        // self.previous_state already had most recent move etc removed from vector by .pop() called previously
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

        if let Some(captured) = move_to_make.captured {
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
        self.update_internal_state_params(&move_to_make)
    }

    pub fn legal_moves_from(&self, start: usize) -> Moves {
        self.legal_moves()
            .into_iter()
            .filter(|m| m.start_square == start)
            .collect()
    }
}
// TODO checking logic (blocking, capturing, double check -> king move, not moving into check)
// TODO castling logic (no castling through check, castling rights)
// TODO that one stupid en passant edge case that's not covered by pin logic
// piece move generators 
impl GameState {
    fn pseudolegal_moves(&self) -> Moves {
        let mut psl_moves: Moves = Vec::new();
        psl_moves.extend(self.pawn_moves(self.side_to_move));
        psl_moves.extend(self.knight_moves(self.side_to_move));
        psl_moves.extend(self.bishop_moves(self.side_to_move));
        psl_moves.extend(self.rook_moves(self.side_to_move));
        psl_moves.extend(self.queen_moves(self.side_to_move));
        psl_moves.extend(self.king_moves(self.side_to_move));
        psl_moves
    }

    fn pawn_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves: Moves = Vec::new();
        let mut pawns = self.bitboards.pieces[piece_colour as usize][0];

        let pins = &self.pins_and_checkers.pins;

        while let Some(start) = pop_lsb(&mut pawns) {
            let mut pin_mask: u64 = !0; // default is to allow all squares
            for pin in pins.iter() {
                if pin.pinned_piece_square == start {
                    pin_mask = pin.ray_mask;
                    break;
                }
            }
            // one square forward
            let one_step: i8 = match piece_colour {
                PieceColour::White => 8,
                PieceColour::Black => -8,
            };

            let end = (start as i8 + one_step) as usize;
            if !(0..64).contains(&end) {
                continue;
            }
            let end_square_breaks_pin = (pin_mask & (1u64 << end)) == 0;

            // in order to move one or two squares, first square must be empty, and pin direction must be vertical
            if is_empty(self.bitboards.occupied, end) & !end_square_breaks_pin {
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
                let end_square_breaks_pin = (pin_mask & (1u64 << end)) == 0;
                // to capture out of a pin, we must be capturing the pinning piece
                if let Some(captured_piece) = self.bitboards.enemy_at_square(end, piece_colour) {
                    if !end_square_breaks_pin {
                        if is_promotion_rank(piece_colour, start) {
                            moves.extend(Move::promotions(start, end, Some(captured_piece)))
                        } else {
                            moves.push(Move::capture(start, end, PieceKind::Pawn, captured_piece))
                        }
                    }
                }

                // en passant
                if let Some(en_passant_square) = self.en_passant_square {
                    if ((1u64 << end & en_passant_square) != 0) && !end_square_breaks_pin {
                        moves.push(Move::en_passant(start, end))
                    }
                }
            }
        }
        moves
    }

    fn knight_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves = Vec::new();
        let mut knights = self.bitboards.pieces[piece_colour as usize][1];
        let pins = &self.pins_and_checkers.pins;

        'loop_over_knights: while let Some(start) = pop_lsb(&mut knights) {
            // if pinned, move to next knight - no legal moves within the pin direction
            for pin in pins.iter() {
                if pin.pinned_piece_square == start {
                    continue 'loop_over_knights
                }
            }

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
                    let captured_piece = self
                        .bitboards
                        .enemy_at_square(end, piece_colour)
                        .expect("Uncapturable piece snuck into knight captures");
                    moves.push(Move::capture(start, end, PieceKind::Knight, captured_piece));
                }
            }
        }
        moves
    }

    fn bishop_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves: Moves = vec![];
        let piece = Piece::new(piece_colour, PieceKind::Bishop);
        let mut bishops = self.bitboards.pieces[piece_colour as usize][2];

        while let Some(start) = pop_lsb(&mut bishops) {
            moves.extend(self.slider_moves(start, piece, BISHOP_DIRECTIONS));
        }
        moves
    }

    fn rook_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves: Moves = vec![];
        let piece = Piece::new(piece_colour, PieceKind::Rook);
        let mut rooks = self.bitboards.pieces[piece_colour as usize][3];

        while let Some(start) = pop_lsb(&mut rooks) {
            moves.extend(self.slider_moves(start, piece, ROOK_DIRECTIONS));
        }
        moves
    }

    fn queen_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves: Moves = vec![];
        let piece = Piece::new(piece_colour, PieceKind::Queen);
        let mut queens = self.bitboards.pieces[piece_colour as usize][4];

        while let Some(start) = pop_lsb(&mut queens) {
            moves.extend(self.slider_moves(start, piece, QUEEN_DIRECTIONS));
        }
        moves
    }

    fn king_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves: Moves = Vec::new();
        let mut king_bb = self.bitboards.pieces[piece_colour as usize][5];
        let start =
            pop_lsb(&mut king_bb).expect("No king of colour {piece_colour} left on the board");
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

            let enemies = self
                .bitboards
                .get_coloured_pieces(1 - piece_colour as usize);

            if is_capturable(enemies, end) {
                let enemy = self.bitboards.enemy_at_square(end, piece_colour);

                moves.push({
                    Move {
                        start_square: start,
                        end_square: end,
                        piece_moved: PieceKind::King,
                        captured: enemy,
                        promotion: None,
                        move_type: MoveType::Normal,
                    }
                })
            }
        }
        moves
    }
}

#[derive(Clone, Copy, Debug)]
struct Pin {
    pinned_piece_square: usize,
    ray_mask: u64,
}
type Pins = Vec<Pin>;
impl Pin {
    fn new(square: usize, mask: u64) -> Pin {
        Pin {
            pinned_piece_square: square,
            ray_mask: mask,
        }
    }
}
#[derive(Clone, Copy, Debug)]
struct Checker {
    checker_piece_square: usize,
    ray_mask: u64,
}
type Checkers = Vec<Checker>;
impl Checker {
    fn new(square: usize, mask: u64) -> Checker {
        Checker {
            checker_piece_square: square,
            ray_mask: mask,
        }
    }
}
#[derive(Clone, Debug)]
struct PinsAndCheckers {
    pins: Pins,
    checkers: Checkers,
}

#[allow(dead_code)]
pub fn print_bitboard(bb: u64) {
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

pub fn usize_to_square_name(square_index: usize) -> String {
    let coord = BoardCoordinate::from_usize(square_index);
    coord.square_name()
}

// pins and checks
impl BitBoards {
    fn friendly_pieces_on_ray(&self, piece_colour: PieceColour, ray: u64) -> u32 {
        (self.get_coloured_pieces(piece_colour as usize) & ray).count_ones()
    }

    fn opposing_king_on_unobstructed_ray(
        &self,
        pinning_side: usize,
        start: usize,
        ray: u64,
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

        println!("ray unobstructed; may pin or check");
        true
    }

    fn get_pins_and_checks(&self, side_to_move: PieceColour) -> PinsAndCheckers {
        // side to move should be the side with the king which may be in check / pieces which may be pinned
        let pinning_side = 1 - side_to_move as usize;
        let mut pins: Pins = Vec::new();
        let mut checkers: Checkers = Vec::new();

        let king_bb = self.pieces[side_to_move as usize][5];
        assert!(
            king_bb.count_ones() == 1,
            "Wrong number of kings on {side_to_move:?} bitboards"
        );
        let king_square = king_bb.trailing_zeros() as usize;

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
                            println!("----------------------------------------");
                            println!("check reached");
                            println!("king square: {}", usize_to_square_name(king_square));
                            println!("attacking slider: {}", usize_to_square_name(start));
                            checkers.push(Checker::new(start, ray_up_to_king));
                            println!("checker successfully pushed");
                            continue 'slider_loop; // don't bother with other directions, as they won't be king-facing
                        }
                        1 => {
                            let pinned_piece_square =
                                (ray_up_to_king & self.get_coloured_pieces(side_to_move as usize))
                                    .trailing_zeros() as usize;
                            pins.push(Pin::new(pinned_piece_square, ray_up_to_king));
                            continue 'slider_loop;
                        }
                        _ => (), // more than one friendly piece in the way; no check or pin
                    }
                }
            }
        }
        PinsAndCheckers { pins, checkers }
    }
}

// gamestate-independent attack mask generation
impl BitBoards {
    fn pawn_attacks(&self, piece_colour: PieceColour) -> u64 {
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

    fn knight_attacks(&self, piece_colour: PieceColour) -> u64 {
        let mut current_knight_attack_masks = 0u64;
        let mut knights = self.pieces[piece_colour as usize][1];

        while let Some(start) = pop_lsb(&mut knights) {
            current_knight_attack_masks |= KNIGHT_ATTACKS[start];
        }
        current_knight_attack_masks
    }

    fn king_attacks(&self, piece_colour: PieceColour) -> u64 {
        let king_bb = self.pieces[piece_colour as usize][5];
        assert!(
            king_bb.count_ones() == 1,
            "King bitboard misfire for {piece_colour:?}"
        );
        let king_position = self.pieces[piece_colour as usize][5].trailing_zeros() as usize;

        KING_ATTACKS[king_position]
    }

    fn directional_attacks(&self, slider_locations: u64, direction: usize) -> u64 {
        let mut current_slider_attack_masks = 0u64;
        let mut sliders = slider_locations;
        let occupied = self.occupied;

        while let Some(start) = pop_lsb(&mut sliders) {
            let ray = ATTACK_MASKS[direction][start];
            let blockers = ray & occupied;

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

    fn slider_attacks(&self, piece_kind: usize, piece_colour: PieceColour) -> u64 {
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
            current_slider_attack_masks |= self.directional_attacks(sliders, direction);
        }
        current_slider_attack_masks
    }

    fn all_attacked_squares(&self, piece_colour: PieceColour) -> u64 {
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

// movegen helpers
impl GameState {
    fn slider_moves(&self, start: usize, piece: Piece, directions: &[usize]) -> Moves {
        let mut moves: Moves = Vec::new();
        let occupied = self.bitboards.occupied;
        let pins = &self.pins_and_checkers.pins;

        for &direction in directions {
            let mut pin_mask: u64 = !0; // default is to allow all squares
            for pin in pins.iter() {
                if pin.pinned_piece_square == start {
                    pin_mask = pin.ray_mask;
                    break;
                }
            }

            let ray = ATTACK_MASKS[direction][start];
            let blockers = ray & occupied;

            if blockers != 0 {
                let blocker_square = closest_blocker(direction, blockers);
                let mut ray_up_to_blocker =
                    ray & mask_up_to_exclusive(start, &direction, blocker_square);

                // append quiet moves
                'target_loop: while let Some(end) = pop_lsb(&mut ray_up_to_blocker) {
                    let target_square_breaks_pin = ((1u64 << end) & pin_mask) == 0;
                    if target_square_breaks_pin { // move breaks pin, try next end square
                        continue 'target_loop
                    }
                    moves.push(Move::quiet(start, end, piece.kind))
                }

                // if blocker is capturable, capture it

                if let Some(target_piece) = self.bitboards.piece_at_square(blocker_square) {
                    let blocker_capture_breaks_pin = ((1u64 << blocker_square) & pin_mask) == 0;
                    if (target_piece.colour != piece.colour) && !blocker_capture_breaks_pin {
                        moves.push(Move::capture(
                            start,
                            blocker_square,
                            piece.kind,
                            target_piece.kind,
                        ));
                    }
                }
            } else {
                let mut ray_open = ray;
                'target_loop: while let Some(end) = pop_lsb(&mut ray_open) {
                    let target_square_breaks_pin = ((1u64 << end) & pin_mask) == 0;
                    if target_square_breaks_pin { // move breaks pin, try next end square
                        continue 'target_loop
                    }
                    moves.push(Move::quiet(start, end, piece.kind))
                }
            }
        }
        moves
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
            let (start, end) = (move_made.start_square as i16, move_made.end_square as i16);
            // should only be true if pawn has moved two squares
            if (end - start) % 16 == 0 {
                // set en_passant_square to be the midpoint of the start and end moves
                self.en_passant_square = Some(1u64 << (start + (end - start) / 2))
            }
        }

        // pass turn over
        match self.side_to_move {
            PieceColour::White => {
                self.side_to_move = PieceColour::Black;
            }
            PieceColour::Black => {
                self.side_to_move = PieceColour::White;
            }
        }

        // recalculate pins and checks
        self.pins_and_checkers = self.bitboards.get_pins_and_checks(self.side_to_move);
    }
}

// misc helpers

fn closest_blocker(direction: usize, blockers: u64) -> usize {
    match direction {
        0..4 => index_lsb(blockers).expect("No LSB found"),
        4..8 => index_msb(blockers).expect("No MSB found"),
        _ => unreachable!("direction index overflow"),
    }
}

fn mask_up_to_exclusive(start: usize, direction: &usize, blocker_square: usize) -> u64 {
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

fn mask_up_to_inclusive(start: usize, direction: &usize, blocker_square: usize) -> u64 {
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

#[derive(Default, Clone)]
pub struct BitBoards {
    pub pieces: [[u64; 6]; 2], // pieces[piece_colour][piece_kind]
    pub white_pieces: u64,
    pub black_pieces: u64,
    occupied: u64,
    attacked_by_white: u64,
    attacked_by_black: u64,
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
    (piece_colour == PieceColour::White) & ((8..16).contains(&start))
        | (piece_colour == PieceColour::Black) & ((48..56).contains(&start))
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

        self.attacked_by_white = self.all_attacked_squares(PieceColour::White);
        self.attacked_by_black = self.all_attacked_squares(PieceColour::Black);
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
                return Some(Piece::new(colour, PieceKind::try_from(kind_index).unwrap()));
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

    fn get_coloured_pieces(&self, piece_colour: usize) -> u64 {
        match piece_colour {
            0 => self.white_pieces,
            1 => self.black_pieces,
            _ => unreachable!("can't look up coloured pieces for non-binary digit"),
        }
    }
}
pub fn bb_to_screen_printable_string(bb: &u64) -> [String; 8] {
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

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Move {
    pub start_square: usize,
    pub end_square: usize,
    pub piece_moved: PieceKind,
    pub captured: Option<PieceKind>,
    pub promotion: Option<PieceKind>,
    pub move_type: MoveType,
}
type Moves = Vec<Move>;
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

    fn promotions(start: usize, end: usize, target_kind: Option<PieceKind>) -> Moves {
        let promotions = [
            PieceKind::Knight,
            PieceKind::Bishop,
            PieceKind::Rook,
            PieceKind::Queen,
        ];
        promotions
            .into_iter()
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

    fn en_passant(start: usize, end: usize) -> Move {
        Move {
            start_square: start,
            end_square: end,
            piece_moved: PieceKind::Pawn,
            captured: Some(PieceKind::Pawn),
            promotion: None,
            move_type: MoveType::EnPassant,
        }
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
