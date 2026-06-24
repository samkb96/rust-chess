use crate::constants::magic::lookup_magic;
use crate::constants::masks::{
    KNIGHT_ATTACKS, TRIMMED_BISHOP_MASKS, TRIMMED_QUEEN_MASKS, TRIMMED_ROOK_MASKS,
};
use crate::game_state::{GameState, Move, PreviousState};
use crate::mechanics::PieceKind::*;
use crate::mechanics::{Piece, PieceColour::*};
use crate::mechanics::{PieceColour, PieceKind};
use crate::movegen::MoveType::{self, *};

type Recalculated = [[bool; 6]; 2];
fn initialise_recalculated() -> Recalculated {
    [[false; 6]; 2]
}

// TODO rewrite everything with piece ids
// TODO implement to make_move and unmake_move - make sure they're in sync with the bitboards
// TODO incremental attack mask update
// TODO incremental pins/checks update?
// TODO incremental movegen

impl GameState {
    pub fn make_move(&mut self, move_to_make: Move) {
        // if we're about to capture a king, print the previous position and the move that led to this position

        if move_to_make.captured() == Some(PieceKind::King) {
            // TODO remove once all the movegen stuff is done - purely here for debugging
            self.unmake_move();
            let legal_moves = self.legal_moves();
            println!("King capture detected. Previous position: ");
            display_bitboards(&self.bitboards.pieces);
            println!("Legal moves:");

            for mv in legal_moves {
                println!("{mv:?}");
            }

            println!("FEN: {}", self.to_fen());
            panic!();
        }
        // first cache state
        self.previous_state.push(PreviousState {
            bitboards: self.bitboards.clone(),
            last_move: move_to_make,
            castling_rights: self.castling_rights,
            en_passant_square: self.en_passant_square,
            halfmove_clock: self.halfmove_clock,
        });

        self.update_position_bitboards(move_to_make);
        self.update_pieces(move_to_make);
        self.update_position_aggregates();
        self.update_aggregates(move_to_make);
        self.update_internal_state_params(move_to_make);
    }

    pub fn unmake_move(&mut self) {
        let previous_state = self
            .previous_state
            .pop()
            .expect("Calling unmake move with empty cache");

        self.bitboards = previous_state.bitboards;
        self.revert_pieces(previous_state.last_move);
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
                // moved piece
                self.locations_from_ids[id_moved as usize] = Some(end_square);
                self.ids_from_locations[start_square as usize] = None;
                self.ids_from_locations[end_square as usize] = Some(id_moved);

                if let Some(id_captured) = id_captured {
                    self.locations_from_ids[id_captured as usize] = None;
                    self.pieces[id_captured as usize] = None;
                }

                if let Some(promotion_choice) = promotion_choice {
                    self.pieces[id_moved as usize]
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
        let side_just_moved = move_made.side_moving();

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
                }

                // turn promoted piece back into pawn
                if promotion_choice.is_some() {
                    self.pieces[id_moved as usize]
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
                self.locations_from_ids[id_moved as usize] = None;

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

                let rook_id = id_captured.unwrap();

                // king
                self.ids_from_locations[end_square as usize] = None;
                self.ids_from_locations[start_square as usize] = Some(id_moved);
                self.locations_from_ids[id_moved as usize] = Some(start_square);

                // rook
                self.ids_from_locations[rook_square_after] = None;
                self.ids_from_locations[rook_square_before] = Some(rook_id);
                self.locations_from_ids[id_moved as usize] = Some(start_square);
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
    }

    fn update_position_bitboards(&mut self, move_to_make: Move) {
        let start_bitboard = 1u64 << move_to_make.start_square();
        let end_bitboard = 1u64 << move_to_make.end_square();

        let piece_moved = move_to_make.piece_moved();
        let captured = move_to_make.piece_moved();

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

    fn is_slider_in_view(
        &self,
        slider_kind: PieceKind,
        piece_colour: PieceColour,
        square_index: usize,
    ) -> bool {
        // test whether a slider can see the king by imagining the king is a slider of the same type, and testing if it sees the slider

        let sliders_to_check = self.bitboards.get(piece_colour, slider_kind);

        if sliders_to_check == 0 {
            return false;
        }; // don't bother with the rest if there are none on the board

        let trimmed_mask = match slider_kind {
            Bishop => TRIMMED_BISHOP_MASKS[square_index],
            Rook => TRIMMED_ROOK_MASKS[square_index],
            Queen => TRIMMED_QUEEN_MASKS[square_index],
            _ => unreachable!(),
        };

        let blocker_config = self.bitboards.occupied & trimmed_mask;
        let attack_mask = lookup_magic(4, square_index, blocker_config);

        (sliders_to_check & attack_mask) != 0
    }

    fn update_sliders_in_view_of_square(
        &mut self,
        square: usize,
        mut recalculated: Recalculated,
    ) -> Recalculated {
        for slider_kind in [Bishop, Rook, Queen] {
            for colour in [White, Black] {
                if recalculated[colour as usize][slider_kind as usize] {
                    continue; // skip if already updated
                }

                if self.bitboards.get(colour, slider_kind) == 0 {
                    continue; // none on the board to recalculate
                }

                if !self.is_slider_in_view(slider_kind, colour, square) {
                    continue; // none in view of square
                }

                recalculated = self.conditional_attack_recalc(slider_kind, colour, recalculated);
            }
        }
        recalculated
    }

    fn update_same_side_sliders_in_view_of_square(
        &mut self,
        square: usize,
        mut recalculated: Recalculated,
    ) -> Recalculated {
        let colour = self.side_to_move;
        for slider_kind in [Rook, Queen] {
            // bishops' attack masks never modified by castling

            if recalculated[colour as usize][slider_kind as usize] {
                continue;
            }

            if self.bitboards.get(colour, slider_kind) == 0 {
                continue; // none on the board to recalculate
            }

            if !self.is_slider_in_view(slider_kind, colour, square) {
                continue;
            }

            recalculated = self.conditional_attack_recalc(slider_kind, colour, recalculated)
        }
        recalculated
    }

    fn update_aggregates_for_normal_move(
        &mut self,
        move_made: Move,
        mut recalculated: Recalculated,
    ) -> Recalculated {
        let piece_moved = move_made.piece_moved();
        let side_to_move = self.side_to_move;
        let start_square = move_made.start_square();
        let end_square = move_made.end_square();

        // recompute only the attacked_from bitboard of the piece that's moved
        recalculated = self.conditional_attack_recalc(piece_moved, side_to_move, recalculated);

        let captured = move_made.captured();
        if let Some(captured) = captured {
            // same match as before, but with colours reversed
            recalculated =
                self.conditional_attack_recalc(captured, side_to_move.flip(), recalculated)
        }

        // indirectly involved pieces (ie blocked/unblocked sliders)

        // for each colour and each slider type, check if there are any in view of the start/end square (without considering blocker)
        for square in [start_square, end_square] {
            recalculated = self.update_sliders_in_view_of_square(square as usize, recalculated)
        }

        recalculated
    }

    fn update_aggregates_for_promotion(
        &mut self,
        promotion_choice: PieceKind,
        end_square: usize,
        recalculated: Recalculated,
    ) {
        let side_to_move = self.side_to_move;

        match promotion_choice {
            Knight => {
                let new_mask = KNIGHT_ATTACKS[end_square];
                self.bitboards.attacked_by[side_to_move as usize][1] |= new_mask
            }
            Bishop | Rook | Queen => {
                // sliders
                let _ =
                    self.conditional_attack_recalc(promotion_choice, side_to_move, recalculated);
            }
            _ => unreachable!("can't promote to pawn or king"),
        };
    }

    fn update_aggregates_for_castling(
        &mut self,
        castling_side: MoveType,
        side_to_move: PieceColour,
        mut recalculated: Recalculated,
    ) {
        let affected_squares = match (side_to_move, castling_side) {
            (White, CastleKingside) => [4, 5, 6, 7],
            (White, CastleQueenside) => [0, 2, 3, 4],
            (Black, CastleKingside) => [60, 61, 62, 63],
            (Black, CastleQueenside) => [56, 58, 59, 60],
            _ => unreachable!("this function's only for castling"),
        };

        recalculated = self.conditional_attack_recalc(King, side_to_move, recalculated);
        recalculated = self.conditional_attack_recalc(Rook, side_to_move, recalculated);
        for square in affected_squares {
            self.update_same_side_sliders_in_view_of_square(square, recalculated);
            // opposing sliders don't need recalculation
        }
    }

    pub fn update_aggregates(&mut self, move_made: Move) {
        let mut recalculated = initialise_recalculated();
        let side_to_move = self.side_to_move;
        let start_square = move_made.start_square();
        let end_square = move_made.end_square();
        let move_type = move_made.move_type();

        match move_type {
            Normal => {
                self.update_aggregates_for_normal_move(move_made, recalculated);

                let promotion_choice = move_made.promotion_choice();
                if let Some(promotion_choice) = promotion_choice {
                    self.update_aggregates_for_promotion(
                        promotion_choice,
                        end_square as usize,
                        recalculated,
                    );
                }
            }
            EnPassant => {
                // en passant
                // basically the same process as normal move, but we need to check if any sliders are unblocked on the captured piece square too
                recalculated = self.update_aggregates_for_normal_move(move_made, recalculated);
                let captured_pawn_square = (start_square & 0xF8) | (end_square & 0x7); // lovely bitop. start rank & end file
                self.update_sliders_in_view_of_square(captured_pawn_square as usize, recalculated);
            }
            CastleKingside | CastleQueenside => {
                self.update_aggregates_for_castling(move_type, side_to_move, recalculated);
            }
        }
        self.bitboards.attacked_by_white = self.bitboards.attacked_by[0]
            .iter()
            .fold(0u64, |acc, elt| acc | elt);
        self.bitboards.attacked_by_black = self.bitboards.attacked_by[1]
            .iter()
            .fold(0u64, |acc, elt| acc | elt);
    }

    fn call_piece_attack_recalc(&mut self, piece_kind: PieceKind, colour: PieceColour) {
        let u_colour = colour as usize;
        let u_kind = piece_kind as usize;
        match piece_kind {
            Pawn => {
                self.bitboards.attacked_by[u_colour][0] = self.bitboards.pawn_attacks(colour);
            }
            Knight => {
                self.bitboards.attacked_by[u_colour][1] = self.bitboards.knight_attacks(colour)
            }
            Bishop | Rook | Queen => {
                self.bitboards.attacked_by[u_colour][u_kind] =
                    self.bitboards.slider_attacks(piece_kind, colour)
            }
            King => self.bitboards.attacked_by[u_colour][5] = self.bitboards.king_attacks(colour),
        }
    }

    fn conditional_attack_recalc(
        &mut self,
        piece_kind: PieceKind,
        colour: PieceColour,
        mut recalculated: Recalculated,
    ) -> Recalculated {
        let u_colour = colour as usize;
        let u_kind = piece_kind as usize;

        if recalculated[u_colour][u_kind] {
            return recalculated; // don't bother updating if it's been done already
        }

        self.call_piece_attack_recalc(piece_kind, colour);
        recalculated[u_colour][u_kind] = true;
        recalculated
    }
}

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
