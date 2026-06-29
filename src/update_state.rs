use crate::constants::magic::lookup_magic;
use crate::constants::masks::*;
use crate::constants::misc::{ROOK_END, ROOK_START};
use crate::game_state::{GameState, Move, PreviousState};
#[allow(unused_imports)]
use crate::mechanics::{BitBoard, Piece, PieceColour::*, pop_lsb, print_bitboard};
use crate::mechanics::{PieceColour, PieceKind, PieceKind::*};
use crate::movegen::MoveType::{self, *};

// TODO incremental pins/checks update

/// attacked_by and aggregates
impl GameState {
    fn get_slider_attack_bitboard(&self, square: usize, kind: PieceKind) -> BitBoard {
        let occupied = self.bitboards.occupied;
        let mask = match kind {
            Bishop => TRIMMED_BISHOP_MASKS[square],
            Rook => TRIMMED_ROOK_MASKS[square],
            Queen => TRIMMED_QUEEN_MASKS[square],
            _ => unreachable!("attacked_by_slider called with non-slider"),
        };

        let blockers = occupied & mask;
        lookup_magic(kind as u8, square, blockers)
    }

    fn calc_attacked_by_id(&self, id: usize) -> BitBoard {
        let Some(piece) = self.pieces[id] else {
            return 0;
        };

        let kind = piece.kind;
        let colour = piece.colour as usize;
        let square = self.locations_from_ids[id].expect("no location at id") as usize;

        match kind {
            Pawn => PAWN_ATTACKS[colour][square],
            Knight => KNIGHT_ATTACKS[square],
            King => KING_ATTACKS[square],
            _ => self.get_slider_attack_bitboard(square, kind),
        }
    }

    fn update_attacked_by_aggregates(&mut self, move_to_make: Move) {
        // incremental approach
        self.update_attacked_by_id_incremental(move_to_make);

        self.bitboards.attacked_by_white = (0..16)
            .map(|id| self.bitboards.attacked_by_id[id])
            .fold(0u64, |acc, elt| acc | elt);

        self.bitboards.attacked_by_black = (16..32)
            .map(|id| self.bitboards.attacked_by_id[id])
            .fold(0u64, |acc, elt| acc | elt);
    }

    fn update_attacked_by_id_incremental(&mut self, move_to_make: Move) {
        // move decoding
        let start = move_to_make.start_square() as usize;
        let end = move_to_make.end_square() as usize;
        let move_type = move_to_make.move_type();
        let id_moved = move_to_make.id_moved() as usize;
        let id_captured = move_to_make.id_captured();

        // track what we've recalculated
        let mut seen = 0u32;

        // new mask for mover
        self.bitboards.attacked_by_id[id_moved] = self.calc_attacked_by_id(id_moved); // knows about promotion if called after update_pieces
        seen |= 1u32 << id_moved;

        // new mask for capturee; early returns 0 if no piece at id anymore
        if let Some(id_captured) = id_captured {
            let id_captured = id_captured as usize;
            self.bitboards.attacked_by_id[id_captured] = self.calc_attacked_by_id(id_captured);
        }

        // recalc slider
        // should handle the rook involved in castling too for free
        self.update_slider_ids(start, end, move_type, seen);
    }

    #[allow(clippy::needless_range_loop)] // don't pull attack masks into iter to avoid reallocation
    fn update_for_increasing_directions(
        &mut self,
        square: usize,
        occupied: BitBoard,
        mut seen: u32,
    ) -> u32 {
        let queens = self.bitboards.pieces[0][4] | self.bitboards.pieces[1][4];
        for direction in 0..4 {
            let ray = ATTACK_MASKS[direction][square];
            let blockers = ray & occupied;
            let horizontal = (direction & 1) == 0;

            if blockers == 0 {
                continue;
            };

            let sliders = if horizontal {
                self.bitboards.rooklike
            } else {
                self.bitboards.bishoplike
            };

            let first_blocker_square = blockers.trailing_zeros() as usize;
            let first_blocker_bb = 1u64 << first_blocker_square;

            if first_blocker_bb & sliders == 0 {
                continue;
            }

            let id = self.ids_from_locations[first_blocker_square].unwrap();
            let id_bit = 1u32 << id;

            if id_bit & seen != 0 {
                // already recalced this slider
                continue;
            }

            // only calc branchy piece kind if we absolutely have to
            let kind = if first_blocker_bb & queens != 0 {
                Queen
            } else if horizontal {
                Rook
            } else {
                Bishop
            };

            seen |= id_bit; // log slider as having been dealt with

            self.bitboards.attacked_by_id[id as usize] =
                self.get_slider_attack_bitboard(first_blocker_square, kind)
        }
        seen
    }

    fn update_for_decreasing_directions(
        &mut self,
        square: usize,
        occupied: BitBoard,
        mut seen: u32,
    ) -> u32 {
        let queens = self.bitboards.pieces[0][4] | self.bitboards.pieces[1][4];
        for direction in 4..8 {
            let ray = ATTACK_MASKS[direction][square];
            let blockers = ray & occupied;

            let horizontal = (direction & 1) == 0;

            if blockers == 0 {
                continue;
            };

            let sliders = if horizontal {
                self.bitboards.rooklike
            } else {
                self.bitboards.bishoplike
            };

            let first_blocker_square = (63 - blockers.leading_zeros()) as usize;
            let first_blocker_bb = 1u64 << first_blocker_square;

            if first_blocker_bb & sliders == 0 {
                continue;
            };

            let id = self.ids_from_locations[first_blocker_square].unwrap();
            let id_bit = 1u32 << id;

            if id_bit & seen != 0 {
                // already recalced this slider
                continue;
            }

            let kind = if first_blocker_bb & queens != 0 {
                Queen
            } else if horizontal {
                Rook
            } else {
                Bishop
            };

            seen |= id_bit; // log slider as dealt with

            self.bitboards.attacked_by_id[id as usize] =
                self.get_slider_attack_bitboard(first_blocker_square, kind)
        }
        seen
    }

    fn update_all_directions(&mut self, square: usize, occupied: BitBoard, mut seen: u32) -> u32 {
        seen = self.update_for_decreasing_directions(square, occupied, seen);
        seen = self.update_for_increasing_directions(square, occupied, seen);

        seen
    }

    fn update_slider_ids(&mut self, start: usize, end: usize, move_type: MoveType, mut seen: u32) {
        let occupied = self.bitboards.occupied;

        for square in [start, end] {
            seen = self.update_all_directions(square, occupied, seen);
        }

        match move_type {
            Normal => (), // hotpath
            EnPassant => {
                let ep_capture_square = (start & 0xF8) | (end & 0x7);
                self.update_all_directions(ep_capture_square, occupied, seen);
            }
            CastleKingside | CastleQueenside => {
                let rook_start = ROOK_START[end];
                let rook_end = ROOK_END[end];
                seen = self.update_all_directions(rook_start, occupied, seen);
                self.update_all_directions(rook_end, occupied, seen);
            }
        };
    }
}

impl GameState {
    pub fn make_move(&mut self, move_to_make: Move) {
        // first cache state
        self.previous_state.push(PreviousState {
            bitboards: self.bitboards.clone(),
            last_move: move_to_make,
            castling_rights: self.castling_rights,
            en_passant_square: self.en_passant_square,
            halfmove_clock: self.halfmove_clock,
        });

        // piece/colour bitboards
        self.update_position_bitboards(move_to_make);

        // piece ids, locations by id, ids by location
        self.update_pieces(move_to_make);

        // white_pieces, black_pieces, occupied, sliders
        self.update_position_aggregates();

        // attacked_by_id, attacked_by_white, attacked_by_black
        self.update_attacked_by_aggregates(move_to_make);

        // castling rights, en passant square, side to move, half/fullmove number
        // also does pins and checks currently. should be moved out
        self.update_internal_state_params(move_to_make);
    }

    pub fn unmake_move(&mut self) {
        let previous_state = self
            .previous_state
            .pop()
            .expect("Calling unmake move with empty cache");

        let move_just_made = previous_state.last_move;

        self.bitboards = previous_state.bitboards;
        self.revert_pieces(move_just_made);
        self.side_to_move = self.side_to_move.flip();
        self.pins_and_checkers = self.bitboards.get_pins_and_checks(self.side_to_move);
        self.castling_rights = previous_state.castling_rights;
        self.en_passant_square = previous_state.en_passant_square;
        self.halfmove_clock = previous_state.halfmove_clock;

        if self.side_to_move == Black {
            self.fullmove_number -= 1 // should only decrement if we're unmaking black's move
        }
    }

    fn update_pieces(&mut self, move_to_make: Move) {
        let start_square = move_to_make.start_square();
        let end_square = move_to_make.end_square();
        let promotion_choice = move_to_make.promotion_choice();
        let move_type = move_to_make.move_type();
        let id_moved = move_to_make.id_moved();
        let id_captured = move_to_make.id_captured();

        let side_moving = self.side_to_move;

        // normal moves

        match move_type {
            Normal => {
                // set ids at locations
                self.ids_from_locations[start_square as usize] = None;
                self.ids_from_locations[end_square as usize] = Some(id_moved);
                // set moving piece location at id
                self.locations_from_ids[id_moved as usize] = Some(end_square);

                // set pieces, locations at ids
                if let Some(id_captured) = id_captured {
                    // capturing - remove old id from pieces + locations
                    self.pieces[id_captured as usize] = None;
                    self.locations_from_ids[id_captured as usize] = None;
                }

                if let Some(promotion_choice) = promotion_choice {
                    // if we're promoting, change the piece kind
                    self.pieces[id_moved as usize]
                        .as_mut()
                        .expect("Promoting pawn should exist")
                        .kind = promotion_choice;
                }
            }

            EnPassant => {
                let id_captured = id_captured.unwrap();

                // move the capturing pawn
                self.locations_from_ids[id_moved as usize] = Some(end_square);
                self.ids_from_locations[start_square as usize] = None;
                self.ids_from_locations[end_square as usize] = Some(id_moved);

                // remove the ep captured pawn
                let ep_captured_rank = start_square & 0xF8;
                let ep_captured_file = end_square & 0x7;
                let ep_captured_square = ep_captured_rank | ep_captured_file;

                self.pieces[id_captured as usize] = None;
                self.locations_from_ids[id_captured as usize] = None;
                self.ids_from_locations[ep_captured_square as usize] = None;
            }
            CastleKingside | CastleQueenside => {
                // castling
                let (mut rook_old_square, mut rook_new_square) = match move_type {
                    CastleKingside => (7usize, 5usize),
                    CastleQueenside => (0usize, 3usize),
                    _ => unreachable!(),
                };

                if side_moving == Black {
                    rook_old_square += 56;
                    rook_new_square += 56;
                };

                let rook_id = self.ids_from_locations[rook_old_square].unwrap();

                // update king
                self.ids_from_locations[start_square as usize] = None;
                self.ids_from_locations[end_square as usize] = Some(id_moved);
                self.locations_from_ids[id_moved as usize] = Some(end_square);

                // update rook
                self.ids_from_locations[rook_old_square] = None;
                self.ids_from_locations[rook_new_square] = Some(rook_id);
                self.locations_from_ids[rook_id as usize] = Some(rook_new_square as u8);
            }
        }
    }

    fn revert_pieces(&mut self, move_made: Move) {
        let start_square = move_made.start_square();
        let end_square = move_made.end_square();
        let captured = move_made.captured();
        let promotion_choice = move_made.promotion_choice();
        let move_type = move_made.move_type();
        let id_moved = move_made.id_moved();
        let id_captured = move_made.id_captured();
        let side_just_moved = self.side_to_move.flip();

        match move_type {
            Normal => {
                // reappend piece to start square
                self.ids_from_locations[start_square as usize] = Some(id_moved);
                self.locations_from_ids[id_moved as usize] = Some(start_square);

                // reappend captured piece
                if let Some(captured) = captured {
                    let id_captured = id_captured.expect("Captured ID missing");
                    let captured_piece = Piece {
                        kind: captured,
                        colour: side_just_moved.flip(),
                        id: id_captured,
                    };
                    self.pieces[id_captured as usize] = Some(captured_piece);
                    self.locations_from_ids[id_captured as usize] = Some(end_square);
                    self.ids_from_locations[end_square as usize] = Some(id_captured);
                } else {
                    // clear end square
                    self.ids_from_locations[end_square as usize] = None;
                }

                // turn promoted piece back into pawn
                if promotion_choice.is_some() {
                    self.pieces[id_moved as usize]
                        .as_mut()
                        .expect("Lost track of promoting pawn's ID")
                        .kind = Pawn;
                }
            }

            EnPassant => {
                let id_captured = id_captured.expect("Lost track of EP captured pawn ID");
                // reappend pawn to start square
                self.ids_from_locations[start_square as usize] = Some(id_moved);
                self.locations_from_ids[id_moved as usize] = Some(start_square);

                // remove pawn from end square
                self.ids_from_locations[end_square as usize] = None;

                // reappend captured pawn
                let ep_captured_rank = start_square & 0xF8;
                let ep_captured_file = end_square & 0x7;
                let ep_captured_square = ep_captured_rank | ep_captured_file;

                self.pieces[id_captured as usize] = Some(Piece {
                    kind: Pawn,
                    colour: side_just_moved.flip(),
                    id: id_captured,
                });
                self.locations_from_ids[id_captured as usize] = Some(ep_captured_square);
                self.ids_from_locations[ep_captured_square as usize] = Some(id_captured);
            }
            CastleKingside | CastleQueenside => {
                let (mut rook_square_before, mut rook_square_after) = match move_type {
                    CastleKingside => (7usize, 5usize),
                    CastleQueenside => (0usize, 3usize),
                    _ => unreachable!(),
                };

                if side_just_moved == Black {
                    rook_square_after += 56;
                    rook_square_before += 56;
                };

                let rook_id =
                    self.ids_from_locations[rook_square_after].expect("No rook to revert");

                // king
                self.ids_from_locations[end_square as usize] = None;
                self.ids_from_locations[start_square as usize] = Some(id_moved);
                self.locations_from_ids[id_moved as usize] = Some(start_square);

                // rook
                self.ids_from_locations[rook_square_after] = None;
                self.ids_from_locations[rook_square_before] = Some(rook_id);
                self.locations_from_ids[rook_id as usize] = Some(rook_square_before as u8);
            }
        }
    }

    pub fn update_internal_state_params(&mut self, move_made: Move) {
        let start_square = move_made.start_square();
        let end_square = move_made.end_square();
        let piece_moved = move_made.piece_moved();
        let captured = move_made.captured();

        // if move not capture & not pawn move, add 1 to halfmove clock
        if (captured.is_none()) & (piece_moved != Pawn) {
            self.halfmove_clock += 1;
        }

        // castling rights lost if rook moves, king moves, rook captured
        if piece_moved == King {
            match self.side_to_move {
                White => {
                    self.castling_rights.white_kingside = false;
                    self.castling_rights.white_queenside = false;
                }
                Black => {
                    self.castling_rights.black_kingside = false;
                    self.castling_rights.black_queenside = false;
                }
            }
        }

        if piece_moved == Rook {
            match start_square {
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

        if captured == Some(Rook) {
            match end_square {
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
        if piece_moved == Pawn {
            // should only be true if pawn has moved two squares
            if end_square.abs_diff(start_square) == 16 {
                // set en_passant_square to be the midpoint of the start and end moves
                self.en_passant_square = Some(1u64 << ((start_square + end_square) / 2))
            } else {
                self.en_passant_square = None
            }
        } else {
            self.en_passant_square = None
        }

        // pass turn over & update fullmove_number if black has just moved
        match self.side_to_move {
            White => {
                self.side_to_move = Black;
            }
            Black => {
                self.fullmove_number += 1;
                self.side_to_move = White;
            }
        }

        // recalculate pins and checks
        self.pins_and_checkers = self.bitboards.get_pins_and_checks(self.side_to_move);
    }

    fn update_position_aggregates(&mut self) {
        self.bitboards.white_pieces = self.bitboards.pieces[0]
            .iter()
            .fold(0u64, |acc, elt| acc | elt);
        self.bitboards.black_pieces = self.bitboards.pieces[1]
            .iter()
            .fold(0u64, |acc, elt| acc | elt);
        self.bitboards.occupied = self.bitboards.white_pieces | self.bitboards.black_pieces;
        self.bitboards.bishoplike = self.bitboards.pieces[0][2]
            | self.bitboards.pieces[0][4]
            | self.bitboards.pieces[1][2]
            | self.bitboards.pieces[1][4];

        self.bitboards.rooklike = self.bitboards.pieces[0][2]
            | self.bitboards.pieces[0][4]
            | self.bitboards.pieces[1][2]
            | self.bitboards.pieces[1][4];
    }

    fn update_position_bitboards(&mut self, move_to_make: Move) {
        let start_bitboard = 1u64 << move_to_make.start_square();
        let end_bitboard = 1u64 << move_to_make.end_square();

        let piece_moved = move_to_make.piece_moved();

        let colour_moved = self.side_to_move;
        let move_type = move_to_make.move_type();

        // remove piece at start
        self.bitboards.pieces[colour_moved as usize][piece_moved as usize] ^= start_bitboard;

        if let Some(captured) = move_to_make.captured()
            && move_type == Normal
        {
            // remove captured piece

            self.bitboards.pieces[1 - (colour_moved as usize)][captured as usize] ^= end_bitboard;
        }

        if let Some(promotion_piece) = move_to_make.promotion_choice() {
            self.bitboards.pieces[colour_moved as usize][promotion_piece as usize] ^= end_bitboard;
        } else {
            self.bitboards.pieces[colour_moved as usize][piece_moved as usize] ^= end_bitboard;
        }

        if [CastleKingside, CastleQueenside].contains(&move_type) {
            // implement the rook move
            let (rook_start, rook_end) = match (colour_moved, move_type) {
                (PieceColour::White, CastleKingside) => (7, 5),
                (PieceColour::Black, CastleKingside) => (63, 61),
                (PieceColour::White, CastleQueenside) => (0, 3),
                (PieceColour::Black, CastleQueenside) => (56, 59),
                _ => unreachable!("Non-castling move snuck into castling logic"),
            };

            // remove rook from original square and add to new square
            let rook_move_bb = (1u64 << rook_start) | (1u64 << rook_end);
            self.bitboards.pieces[colour_moved as usize][PieceKind::Rook as usize] ^= rook_move_bb;
        };

        if move_type == EnPassant {
            // en passant
            // pawn to remove is on file of start square + rank of end
            let start_rank = move_to_make.start_square() & 0xF8; // 00000111
            let end_file = move_to_make.end_square() & 0x7; // 11111000
            let captured_pawn_bit = 1 << (start_rank | end_file);

            // remove captured pawn from opposition bitboard
            self.bitboards.pieces[colour_moved.flip() as usize][0] ^= captured_pawn_bit;
        }
    }
}

#[allow(dead_code)]
fn display_bitboards(bitboards: &[[u64; 6]; 2]) {
    println!("  a b c d e f g h");
    for rank in (0..8).rev() {
        print!("{} ", rank + 1);
        for file in 0..8 {
            let square = rank * 8 + file;
            let square_bit = 1u64 << square;

            let mut piece_char = '.';

            // Check white pieces (uppercase)
            for piece_kind in 0..6 {
                if (bitboards[0][piece_kind] & square_bit) != 0 {
                    piece_char = match piece_kind {
                        0 => 'P',
                        1 => 'N',
                        2 => 'B',
                        3 => 'R',
                        4 => 'Q',
                        5 => 'K',
                        _ => unreachable!(),
                    };
                    break;
                }
            }

            // Check black pieces (lowercase)
            for piece_kind in 0..6 {
                if (bitboards[1][piece_kind] & square_bit) != 0 {
                    piece_char = match piece_kind {
                        0 => 'p',
                        1 => 'n',
                        2 => 'b',
                        3 => 'r',
                        4 => 'q',
                        5 => 'k',
                        _ => unreachable!(),
                    };
                    break;
                }
            }

            print!("{piece_char} ");
        }
        println!(" {}", rank + 1);
    }
    println!("  a b c d e f g h");
}
