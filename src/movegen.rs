use crate::constants::magic::lookup_magic;
use crate::constants::masks::*;
use crate::constants::misc::CASTLING_SQUARES;
use crate::game_state::{GameState, Move, Moves};
use crate::mechanics::PieceColour::{Black, White};
use crate::mechanics::PieceKind::{Bishop, Queen, Rook};
use crate::mechanics::{BitBoard, Piece, PieceColour, PieceKind, pop_lsb};
use smallvec::SmallVec;

const MOVES_VEC_CAPACITY: usize = 64;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum MoveType {
    Normal,
    CastleKingside,
    CastleQueenside,
    EnPassant,
}

impl TryFrom<u8> for MoveType {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(MoveType::Normal),
            1 => Ok(MoveType::EnPassant),
            2 => Ok(MoveType::CastleKingside),
            3 => Ok(MoveType::CastleQueenside),
            _ => Err(()),
        }
    }
}

// piece move generators
impl GameState {
    pub fn legal_moves(&self) -> Moves {
        let mut moves: Moves = SmallVec::with_capacity(MOVES_VEC_CAPACITY);
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
            .filter(|m| m.start_square() == start)
            .collect()
    }

    fn pawn_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves: Moves = SmallVec::with_capacity(MOVES_VEC_CAPACITY);

        let pin_array = self.pins_and_checkers.pins;
        let check_mask = self.pins_and_checkers.check_mask;

        if check_mask == 0 {
            return moves; // double check, no legal pawn moves
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
                        if is_promotion_rank(piece_colour, end) {
                            moves.extend(Move::promotions(start, end, Some(captured_piece)))
                        } else {
                            moves.push(Move::capture(start, end, PieceKind::Pawn, captured_piece));
                        }
                    }
                }

                // en passant
                if let Some(en_passant_square) = self.en_passant_square {
                    // need a function for that stupid edge case
                    if (1u64 << end & en_passant_square) != 0 {
                        // is the end square the EP square
                        if !end_square_illegal && !self.en_passant_double_pin_situation(start) {
                            moves.push(Move::en_passant(start, end));
                        }
                        if self.en_passant_capture_of_a_checking_pawn(start) {
                            // this illegal move isn't actually illegal
                            moves.push(Move::en_passant(start, end))
                        }
                    }
                }
            }
        }
        moves
    }

    fn en_passant_capture_of_a_checking_pawn(&self, start: usize) -> bool {
        if self.pins_and_checkers.check_mask == !0 {
            return false;
        }; // need to be in check

        if self.pins_and_checkers.check_mask == 0 {
            return false;
        }; // can't be double check

        let side_to_move = self.side_to_move;

        let en_passant_square_index = self
            .en_passant_square
            .expect("EP square exists within EP capture function")
            .trailing_zeros() as usize;

        let en_passant_capturable_piece_bitboard = match side_to_move {
            PieceColour::White => 1u64 << (en_passant_square_index - 8), // black pawn behind EP square
            PieceColour::Black => 1u64 << (en_passant_square_index + 8), // white pawn ahead of EP square
        };

        if en_passant_capturable_piece_bitboard & self.pins_and_checkers.check_mask == 0 {
            return false; // EP capturable pawn must be the thing checking
        }

        // also need to check that the pawn doing the capture isn't pinned
        if self.pins_and_checkers.pins[start] != !0 {
            return false;
        }

        // if it's passed all these tests, then EP capture of checking pawn should be legal
        true
    }

    fn en_passant_double_pin_situation(&self, start: usize) -> bool {
        let side_to_move = self.side_to_move as usize;
        let opposing_side = 1 - side_to_move;
        let start_rank = start / 8;

        let rank_bitboard = 255u64 << (8 * start_rank) as BitBoard; // 255 is rank 0

        // are we on same rank as our king
        let king_bitboard = self.bitboards.pieces[side_to_move][5];
        if king_bitboard & rank_bitboard == 0 {
            return false;
        };

        // are there enemy rooks/queens on this rank
        let enemy_rooks = self.bitboards.pieces[opposing_side][3];
        let enemy_queens = self.bitboards.pieces[opposing_side][4];
        let mut enemy_horizontal_sliders_on_rank = (enemy_rooks | enemy_queens) & rank_bitboard;

        if enemy_horizontal_sliders_on_rank == 0 {
            return false;
        }

        // check rays under attack towards king of pieces on this rank

        let our_pawn_bitboard = 1u64 << start;
        let our_king_index = king_bitboard.trailing_zeros() as usize;
        while let Some(enemy_slider_index) = pop_lsb(&mut enemy_horizontal_sliders_on_rank) {
            // are we between the slider and the king
            let ray_between_slider_and_king =
                MASK_UP_TO_EXCLUSIVE[our_king_index][enemy_slider_index];
            if ray_between_slider_and_king & our_pawn_bitboard == 0 {
                return false;
            }

            // are the two pawns involved in the en passant the only things on the ray;
            let all_pieces = self.bitboards.occupied;
            if (all_pieces & ray_between_slider_and_king).count_ones() == 2 {
                return true; // somehow, it's that incredibly rare situation
            }
        }
        false
    }

    fn knight_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves = SmallVec::with_capacity(MOVES_VEC_CAPACITY);
        let mut knights = self.bitboards.pieces[piece_colour as usize][1];
        let pin_array = self.pins_and_checkers.pins;
        let check_mask = self.pins_and_checkers.check_mask;

        'knights: while let Some(start) = pop_lsb(&mut knights) {
            // if pinned, move to next knight - no legal moves within the pin direction
            if pin_array[start] != !0 {
                continue 'knights;
            }

            // get attack board for square
            let mut knight_mask = KNIGHT_ATTACKS[start];
            // loop over possible target squares
            'targets: while let Some(end) = pop_lsb(&mut knight_mask) {
                let end_square_illegal = (1u64 << end & check_mask) == 0;

                if end_square_illegal {
                    continue 'targets;
                }

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

    fn slider_moves(&self, start: usize, piece_colour: PieceColour, piece_kind: PieceKind) -> Moves {
        let mut moves: Moves = SmallVec::with_capacity(MOVES_VEC_CAPACITY);
        let occupied = self.bitboards.occupied;
        let pin_and_check_mask =
            self.pins_and_checkers.pins[start] & self.pins_and_checkers.check_mask;

        let (friendly_pieces, enemy_pieces) = match piece_colour {
            White => (self.bitboards.white_pieces, self.bitboards.black_pieces),
            Black => (self.bitboards.black_pieces, self.bitboards.white_pieces),
        };

        let potential_attacks = match piece_kind {
            Bishop => TRIMMED_BISHOP_MASKS[start],
            Rook => TRIMMED_ROOK_MASKS[start],
            Queen => TRIMMED_QUEEN_MASKS[start],
            _ => unreachable!(),
        };

        let blockers = potential_attacks & occupied;
        let mut attack_mask = lookup_magic(piece_kind as u8, start, blockers);

        attack_mask &= pin_and_check_mask; // can't move out of a pin etc
        attack_mask &= !friendly_pieces; // can't capture own pieces

        let mut capturable_mask = attack_mask & enemy_pieces;
        let mut quiet_mask = attack_mask & !enemy_pieces;

        while let Some(end) = pop_lsb(&mut capturable_mask) {
            let captured_piece = self
                .piece_at_square(end)
                .expect("Filtered for squares occupied by enemies already");
            moves.push(Move::capture(start, end, piece_kind, captured_piece.kind))
        }
        while let Some(end) = pop_lsb(&mut quiet_mask) {
            moves.push(Move::quiet(start, end, piece_kind))
        }
        moves
    }

    fn bishop_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves: Moves = SmallVec::with_capacity(MOVES_VEC_CAPACITY);
        let mut bishops = self.bitboards.pieces[piece_colour as usize][2];

        while let Some(start) = pop_lsb(&mut bishops) {
            moves.extend(self.slider_moves(start, piece_colour, Bishop));
        }
        moves
    }

    fn rook_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves: Moves = SmallVec::with_capacity(MOVES_VEC_CAPACITY);
        let mut rooks = self.bitboards.pieces[piece_colour as usize][3];

        while let Some(start) = pop_lsb(&mut rooks) {
            moves.extend(self.slider_moves(start, piece_colour, Rook));
        }
        moves
    }

    fn queen_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves: Moves = SmallVec::with_capacity(MOVES_VEC_CAPACITY);
        let mut queens = self.bitboards.pieces[piece_colour as usize][4];

        while let Some(start) = pop_lsb(&mut queens) {
            moves.extend(self.slider_moves(start, piece_colour, Queen));
        }
        moves
    }

    fn king_moves(&self, piece_colour: PieceColour) -> Moves {
        let mut moves: Moves = SmallVec::with_capacity(MOVES_VEC_CAPACITY);
        let mut king_bb = self.bitboards.pieces[piece_colour as usize][5];

        let mut enemy_attacks = match piece_colour {
            PieceColour::White => self.bitboards.attacked_by_black,
            PieceColour::Black => self.bitboards.attacked_by_white,
        };

        let start =
            pop_lsb(&mut king_bb).expect("No king of colour {piece_colour} left on the board");
        let mut attack_mask = KING_ATTACKS[start];

        // if king in check, we need to recalc slider attack masks using the phantom approach - look up bitboard with king removed from blocker list
        // otherwise, we would allow stepping away from (but still in line with) checking sliders
        let check_mask = self.pins_and_checkers.check_mask;

        if check_mask == 0 {
            enemy_attacks =
                self.recalc_enemy_attacks_double_check(start, piece_colour, enemy_attacks)
        } else if check_mask != !0 {
            enemy_attacks = self.recalc_enemy_attacks_single_check(
                start,
                piece_colour,
                check_mask,
                enemy_attacks,
            )
        }

        let enemies = self
            .bitboards
            .get_coloured_pieces(piece_colour.flip() as usize);

        while let Some(end) = pop_lsb(&mut attack_mask) {
            let moving_into_check = ((1u64 << end) & enemy_attacks) != 0;
            if moving_into_check {
                continue;
            }

            if is_empty(self.bitboards.occupied, end) {
                moves.push(Move::quiet(start, end, PieceKind::King));
            };

            if is_capturable(enemies, end) {
                let enemy = self
                    .bitboards
                    .enemy_at_square(end, piece_colour)
                    .expect("Should never fail to unwrap enemy at capturable square");
                moves.push(Move::capture(start, end, PieceKind::King, enemy));
            }
        }

        // castling
        for (side_id, castling_side) in [(0, 3), (1, 2)].iter() {
            let castling_rights_valid = self
                .castling_rights
                .check(piece_colour as u8, *castling_side);

            let obstruction_squares = CASTLING_SQUARES.0[piece_colour as usize][*side_id];
            let prevented_by_obstruction = (obstruction_squares & self.bitboards.occupied) != 0;

            let vulnerable_squares = CASTLING_SQUARES.1[piece_colour as usize][*side_id];
            let prevented_by_attack = (vulnerable_squares & enemy_attacks) != 0;

            if castling_rights_valid && !prevented_by_obstruction && !prevented_by_attack {
                let king_target = match (piece_colour, castling_side) {
                    (PieceColour::White, 2) => 6,
                    (PieceColour::Black, 2) => 62,
                    (PieceColour::White, 3) => 2,
                    (PieceColour::Black, 3) => 58,
                    _ => unreachable!("Non-castling move in castling generation"),
                };

                moves.push(Move::castling(king_target, *castling_side));
            }
        }
        moves
    }

    fn recalc_enemy_attacks_single_check(
        &self,
        start: usize,
        piece_colour: PieceColour,
        check_mask: BitBoard,
        enemy_attacks: BitBoard,
    ) -> BitBoard {
        // need to extend slider rays through our king to rule out retreats
        // in single check, we already know the sliders attacking our king via enemy_sliders & check_mask
        let enemy_sliders = (2..=4)
            .map(|piece_kind| self.bitboards.pieces[piece_colour.flip() as usize][piece_kind])
            .fold(0u64, |acc, elt| acc | elt);

        let checking_slider = enemy_sliders & check_mask;

        if checking_slider == 0 {
            return enemy_attacks; // checked by a knight / pawn - nothing to recalc
        }

        let slider_index = checking_slider.trailing_zeros() as usize;
        let slider_kind = self
            .piece_at_square(slider_index)
            .expect("No slider found in attack recalc")
            .kind;

        let trimmed_slider_mask = match slider_kind {
            PieceKind::Bishop => TRIMMED_BISHOP_MASKS[slider_index],
            PieceKind::Rook => TRIMMED_ROOK_MASKS[slider_index],
            PieceKind::Queen => TRIMMED_QUEEN_MASKS[slider_index],
            _ => unreachable!(),
        };

        let king_bit = 1u64 << start;
        let blockers_no_king = self.bitboards.occupied & !king_bit;
        let relevant_blockers = trimmed_slider_mask & blockers_no_king;

        let phantom_slider_mask = lookup_magic(slider_kind as u8, slider_index, relevant_blockers);

        enemy_attacks | phantom_slider_mask
    }

    fn recalc_enemy_attacks_double_check(
        &self,
        start: usize,
        piece_colour: PieceColour,
        mut enemy_attacks: BitBoard,
    ) -> BitBoard {
        // in double check, we need to calculate which sliders are attacking
        let king_bb = self.bitboards.pieces[piece_colour as usize][5];
        let enemy_sliders = (2..=4)
            .map(|piece_kind| self.bitboards.pieces[piece_colour.flip() as usize][piece_kind])
            .fold(0u64, |acc, elt| acc | elt);

        // to find sliders in view of king, pretend the king is a queen, and see what it would be attacking
        let rays_from_king = TRIMMED_QUEEN_MASKS[start];
        let blockers = self.bitboards.occupied & rays_from_king;

        let possible_sliders_attacking_king = lookup_magic(4, start, blockers);
        let mut relevant_sliders = possible_sliders_attacking_king & enemy_sliders;

        while let Some(slider_index) = pop_lsb(&mut relevant_sliders) {
            let slider_kind = self
                .piece_at_square(slider_index)
                .expect("no slider found in same side attack update")
                .kind;
            let trimmed_mask = match slider_kind {
                PieceKind::Bishop => TRIMMED_BISHOP_MASKS[slider_index],
                PieceKind::Rook => TRIMMED_ROOK_MASKS[slider_index],
                PieceKind::Queen => TRIMMED_QUEEN_MASKS[slider_index],
                _ => unreachable!(),
            };

            let relevant_blockers = trimmed_mask & blockers & !king_bb; // remove king from blockers for phantom

            let phantom_slider_attacks =
                lookup_magic(slider_kind as u8, slider_index, relevant_blockers);

            enemy_attacks |= phantom_slider_attacks
        }
        enemy_attacks
    }
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
