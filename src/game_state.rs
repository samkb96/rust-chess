
use crate::attack_masks::masks::*;
use crate::game::*;
use crate::mechanics::*;
use crate::constants::*;
use arrayvec::ArrayVec;
use macroquad::prelude::*;
use std::io::*;
#[derive(Clone)]
pub struct GameState {
    pub bitboards: BitBoards,
    pins_and_checkers: PinsAndCheckers,
    side_to_move: PieceColour,
    castling_rights: CastlingRights,
    en_passant_square: Option<BitBoard>,
    halfmove_clock: u16,
    fullmove_number: u16,
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
                    continue
                }, // new rank indicator
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
            _ => panic!("Fen string side to move invalid")
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

    pub fn initialise() -> Self {
        Self::from_fen(INITIALISATION_FEN)
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

        if let Some(captured) = move_to_make.captured && move_type == MoveType::Normal {
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
                true => prev_ep_square >> 8, // rank below ep square if black's side of board
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

    pub fn naive_hash(&self) -> u128 {
        let mut h = 0u128;
        for colour in 0..2 {
            for piece in 0..6 {
                h ^= (self.bitboards.pieces[colour][piece] as u128) << (piece * colour);
            }
        }
        h ^= (self.castling_rights.to_u8() as u128) << 64;
        h ^= self.en_passant_square.unwrap_or(0) as u128;
        h ^= (self.side_to_move as u128) << 37;
        h
    }

}
    fn format_movestring(mv: Move) -> String {
        let start_str = BoardCoordinate::from_usize(mv.start_square).square_name();
        let end_str = BoardCoordinate::from_usize(mv.end_square).square_name();
        let pieces = ["Pawn", "Knight", "Bishop", "Rook", "Queen", "King"];
        let piece_str = pieces[mv.piece_moved as usize];
        let captured_str = if let Some(captured_piece) = mv.captured {
            pieces[captured_piece as usize]
        } else {
            "None"
        };
        format!("Move: {start_str}{end_str}. Piece moved: {piece_str}. Captured: {captured_str}")
    }


// TODO castling logic (no castling through check, castling rights)
// TODO that one stupid en passant edge case that's not covered by pin logic
// piece move generators 
impl GameState {
    pub fn legal_moves(&self) -> Moves {
        let mut moves: Moves = ArrayVec::new();
        moves.extend(self.pawn_moves(self.side_to_move));
        moves.extend(self.knight_moves(self.side_to_move));
        moves.extend(self.bishop_moves(self.side_to_move));
        moves.extend(self.rook_moves(self.side_to_move));
        moves.extend(self.queen_moves(self.side_to_move));
        moves.extend(self.king_moves(self.side_to_move));
        moves
    }
    
    pub fn legal_moves_from(&self, start: usize) -> Moves {
        self.legal_moves()
            .into_iter()
            .filter(|m| m.start_square == start)
            .collect()
    }

    fn pawn_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves: Moves = ArrayVec::new();

        let pin_array = self.pins_and_checkers.pins;
        let check_mask = self.pins_and_checkers.check_mask;

        if check_mask == 0 {
            return moves // double check, no legal pawn moves
        }

        let mut pawns = self.bitboards.pieces[piece_colour as usize][0];

        while let Some(start) = pop_lsb(&mut pawns) {

            let pin_and_check_mask = pin_array[start] & check_mask;

            // one square forward
            let one_step: i8 = match piece_colour {
                PieceColour::White => 8,
                PieceColour::Black => -8,
            };

            let end = (start as i8 + one_step) as usize;
            if !(0..64).contains(&end) {
                continue;
            }
            let end_square_illegal = (pin_and_check_mask & (1u64 << end)) == 0;
            // in order to move one or two squares, first square must be empty, and pin direction must be vertical
            if is_empty(self.bitboards.occupied, end) {
                if !end_square_illegal {
                    if is_promotion_rank(piece_colour, end) {
                        moves.extend(Move::promotions(start, end, None))
                    } else {
                        moves.push(Move::quiet(start, end, PieceKind::Pawn));
                    }
                }

                // two squares behaviour within check to see if first square empty
                if is_double_push_rank(piece_colour, start) {
                    let end = (end as i8 + one_step) as usize;
                    let end_square_illegal = (pin_and_check_mask & (1u64 << end)) == 0;
                    if is_empty(self.bitboards.occupied, end) && !end_square_illegal {
                        moves.push(Move::quiet(start, end, PieceKind::Pawn));
                    }
                }
            }

            let mut pawn_attack_mask = match piece_colour {
                PieceColour::White => WHITE_PAWN_ATTACKS[start],
                PieceColour::Black => BLACK_PAWN_ATTACKS[start],
            };

            while let Some(end) = pop_lsb(&mut pawn_attack_mask) {
                let end_square_illegal = (pin_and_check_mask & (1u64 << end)) == 0;
                // to capture out of a pin, we must be capturing the pinning piece
                if let Some(captured_piece) = self.bitboards.enemy_at_square(end, piece_colour) {
                    if !end_square_illegal {
                        if is_promotion_rank(piece_colour, start) {
                            moves.extend(Move::promotions(start, end, Some(captured_piece)))
                        } else {
                            moves.push(Move::capture(start, end, PieceKind::Pawn, captured_piece));
                        }
                    }
                }

                // en passant
                if let Some(en_passant_square) = self.en_passant_square {
                    if ((1u64 << end & en_passant_square) != 0) && !end_square_illegal {
                        moves.push(Move::en_passant(start, end));
                    }
                }
            }
        }
        moves
    }

    fn knight_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves = ArrayVec::new();
        let mut knights = self.bitboards.pieces[piece_colour as usize][1];
        let pin_array = self.pins_and_checkers.pins;
        let check_mask = self.pins_and_checkers.check_mask;

        'loop_over_knights: while let Some(start) = pop_lsb(&mut knights) {
            // if pinned, move to next knight - no legal moves within the pin direction
            if pin_array[start] != !0 {
                continue 'loop_over_knights
            }

            // get attack board for square
            let mut knight_mask = KNIGHT_ATTACKS[start];
            // loop over possible target squares
            while let Some(end) = pop_lsb(&mut knight_mask) {
                let end_square_illegal = (1u64 << end & check_mask) == 0;
                if is_empty(self.bitboards.occupied, end) && !end_square_illegal {
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

    fn slider_moves(&self, start: usize, piece: Piece, directions: &[usize]) -> Moves {
        let mut moves: Moves = ArrayVec::new();
        let occupied = self.bitboards.occupied;
        let pin_array = self.pins_and_checkers.pins;
        let check_mask = self.pins_and_checkers.check_mask;

        for &direction in directions {
            let pin_and_check_mask = pin_array[start] & check_mask; 

            let ray = ATTACK_MASKS[direction][start];
            let blockers = ray & occupied;

            if blockers != 0 {
                let blocker_square = closest_blocker(direction, blockers);
                let mut ray_up_to_blocker =
                    ray & mask_up_to_exclusive(start, &direction, blocker_square);

                // append quiet moves
                'target_loop: while let Some(end) = pop_lsb(&mut ray_up_to_blocker) {
                    let target_square_illegal = ((1u64 << end) & pin_and_check_mask) == 0;
                    if target_square_illegal { // move breaks pin, try next end square
                        continue 'target_loop
                    }
                    moves.push(Move::quiet(start, end, piece.kind))
                }

                // if blocker is capturable, capture it

                if let Some(target_piece) = self.bitboards.piece_at_square(blocker_square) {
                    let target_square_illegal = ((1u64 << blocker_square) & pin_and_check_mask) == 0;
                    if (target_piece.colour != piece.colour) && !target_square_illegal {
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
                    let target_square_illegal = ((1u64 << end) & pin_and_check_mask) == 0;
                    if target_square_illegal { // move breaks pin, try next end square
                        continue 'target_loop
                    }
                    moves.push(Move::quiet(start, end, piece.kind))
                }
            }
        }
        moves
    }

    fn bishop_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves: Moves = ArrayVec::new();
        let piece = Piece::new(piece_colour, PieceKind::Bishop);
        let mut bishops = self.bitboards.pieces[piece_colour as usize][2];

        while let Some(start) = pop_lsb(&mut bishops) {
            moves.extend(self.slider_moves(start, piece, BISHOP_DIRECTIONS));
        }
        moves
    }

    fn rook_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves: Moves = ArrayVec::new();
        let piece = Piece::new(piece_colour, PieceKind::Rook);
        let mut rooks = self.bitboards.pieces[piece_colour as usize][3];

        while let Some(start) = pop_lsb(&mut rooks) {
            moves.extend(self.slider_moves(start, piece, ROOK_DIRECTIONS));
        }
        moves
    }

    fn queen_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves: Moves = ArrayVec::new();
        let piece = Piece::new(piece_colour, PieceKind::Queen);
        let mut queens = self.bitboards.pieces[piece_colour as usize][4];

        while let Some(start) = pop_lsb(&mut queens) {
            moves.extend(self.slider_moves(start, piece, QUEEN_DIRECTIONS));
        }
        moves
    }

    fn king_moves(&self, piece_colour: PieceColour) -> Moves {
        // TODO prevent castling through check
        let mut moves: Moves = ArrayVec::new();
        let mut king_bb = self.bitboards.pieces[piece_colour as usize][5];

        let enemy_attacks = match piece_colour {
            PieceColour::White => self.bitboards.attacked_by_black,
            PieceColour::Black => self.bitboards.attacked_by_white,
        };

        let start = pop_lsb(&mut king_bb)
            .expect("No king of colour {piece_colour} left on the board");
        let mut attack_mask = KING_ATTACKS[start];

        while let Some(end) = pop_lsb(&mut attack_mask) {
            let moving_into_check = ((1u64 << end) & enemy_attacks) != 0;
            if moving_into_check {
                continue
            }
            
            if is_empty(self.bitboards.occupied, end) {
                moves.push(Move::quiet(start, end, PieceKind::King));
            };

            let enemies = self
                .bitboards
                .get_coloured_pieces(1 - piece_colour as usize);

            if is_capturable(enemies, end) {
                let enemy = self.bitboards.enemy_at_square(end, piece_colour)
                    .expect("Should never fail to unwrap enemy at capturable square");
                moves.push(Move::capture(start, end, PieceKind::King, enemy));
            }
        }
        
        // castling 
        for (side_id, castling_side) in [
            (0, MoveType::CastleQueenside), 
            (1, MoveType::CastleKingside),
            ].iter() {
            let castling_rights_valid = self.castling_rights.check(piece_colour, *castling_side);

            let obstruction_squares = CASTLING_SQUARES.0[piece_colour as usize][*side_id];
            let prevented_by_obstruction = (obstruction_squares & self.bitboards.occupied) != 0;

            let vulnerable_squares = CASTLING_SQUARES.1[piece_colour as usize][*side_id];
            let prevented_by_attack = (vulnerable_squares & enemy_attacks) != 0;


            if castling_rights_valid && !prevented_by_obstruction && !prevented_by_attack {
                let king_target= match (piece_colour, castling_side) {
                    (PieceColour::White, MoveType::CastleKingside) => 6,
                    (PieceColour::Black, MoveType::CastleKingside) => 62,
                    (PieceColour::White, MoveType::CastleQueenside) => 2,
                    (PieceColour::Black, MoveType::CastleQueenside) => 58,
                    _ => unreachable!("Non-castling move in castling generation"),
                };
                
                moves.push(Move::castling(king_target, *castling_side));
            }
        }

        moves
    }
}

#[allow(dead_code)]
pub fn usize_to_square_name(square_index: usize) -> String {
    let coord = BoardCoordinate::from_usize(square_index);
    coord.square_name()
}

// misc helpers

fn is_empty(occupied: BitBoard, square_index: usize) -> bool {
    (occupied & (1u64 << square_index)) == 0
}

fn is_capturable(enemies: BitBoard, square_index: usize) -> bool {
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

    pub fn capture(start: usize, end: usize, piece_kind: PieceKind, target_kind: PieceKind) -> Move {
        Move {
            start_square: start,
            end_square: end,
            piece_moved: piece_kind,
            captured: Some(target_kind),
            promotion: None,
            move_type: MoveType::Normal,
        }
    }
    
    pub fn promotion(start: usize, end: usize, promotion_choice: PieceKind, target_kind: Option<PieceKind>) -> Move {
        Move {
            start_square: start,
            end_square: end, 
            piece_moved: PieceKind::Pawn,
            captured: target_kind,
            promotion: Some(promotion_choice),
            move_type: MoveType::Normal
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
            2|6 => 4,
            58|62 => 60,
            _ => unreachable!("invalid target square for castling")
        };

        Move {
            start_square: start,
            end_square: end,
            piece_moved: PieceKind::King,
            captured: None,
            promotion: None,
            move_type: castling_side
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

fn debug_move(game_state: &mut GameState, m: Move) {
    use std::fmt::Write;

    fn bitboard_str(bb: u64) -> String {
        let mut s = String::with_capacity(64);
        for rank in (0..8).rev() {
            for file in 0..8 {
                let sq = rank * 8 + file;
                s.push(if (bb >> sq) & 1 != 0 { 'X' } else { '.' });
            }
            s.push('\n');
        }
        s
    }

    let side_to_move = game_state.side_to_move as usize;
    let side_just_moved = 1 - side_to_move;

    // Snapshot before move
    println!("=== BEFORE MOVE ===");
    println!("Move: {:?}", m);
    for piece in 0..6 {
        let bb = game_state.bitboards.pieces[side_just_moved][piece];
        if bb != 0 {
            println!(
                "Moving side piece {:?}:\n{}",
                ["Pawn","Knight","Bishop","Rook","Queen","King"][piece],
                bitboard_str(bb)
            );
        }
    }
    if let Some(captured) = m.captured {
        let bb = game_state.bitboards.pieces[side_to_move][captured as usize];
        println!(
            "Captured piece {:?}:\n{}",
            ["Pawn","Knight","Bishop","Rook","Queen","King"][captured as usize],
            bitboard_str(bb)
        );
    }

    // Apply move
    game_state.make_move(m);

    println!("=== AFTER MAKE ===");
    for piece in 0..6 {
        let bb = game_state.bitboards.pieces[side_just_moved][piece];
        if bb != 0 {
            println!(
                "Moving side piece {:?}:\n{}",
                ["Pawn","Knight","Bishop","Rook","Queen","King"][piece],
                bitboard_str(bb)
            );
        }
    }
    if let Some(captured) = m.captured {
        let bb = game_state.bitboards.pieces[side_to_move][captured as usize];
        println!(
            "Captured piece {:?}:\n{}",
            ["Pawn","Knight","Bishop","Rook","Queen","King"][captured as usize],
            bitboard_str(bb)
        );
    }

    // Undo move
    game_state.unmake_move();

    println!("=== AFTER UNMAKE ===");
    for piece in 0..6 {
        let bb = game_state.bitboards.pieces[side_just_moved][piece];
        if bb != 0 {
            println!(
                "Moving side piece {:?}:\n{}",
                ["Pawn","Knight","Bishop","Rook","Queen","King"][piece],
                bitboard_str(bb)
            );
        }
    }
    if let Some(captured) = m.captured {
        let bb = game_state.bitboards.pieces[side_to_move][captured as usize];
        println!(
            "Captured piece {:?}:\n{}",
            ["Pawn","Knight","Bishop","Rook","Queen","King"][captured as usize],
            bitboard_str(bb)
        );
    }
}
