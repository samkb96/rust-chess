use crate::constants::magic::lookup_magic;
use crate::constants::masks::{
    KNIGHT_ATTACKS, TRIMMED_BISHOP_MASKS, TRIMMED_QUEEN_MASKS, TRIMMED_ROOK_MASKS
};
use crate::constants::misc::{PIECE_COLOURS, SLIDERS};
use crate::game_state::{GameState, Move, PreviousState};
use crate::mechanics::{PieceColour, PieceKind};
use crate::movegen::MoveType;

type Recalculated = [[bool; 6]; 2];
fn initialise_recalculated() -> Recalculated {
    [[false; 6]; 2]
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

        self.update_position_bitboards(move_to_make);
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
        self.side_to_move = self.side_to_move.flip();
        self.pins_and_checkers = self.bitboards.get_pins_and_checks(self.side_to_move);

        self.castling_rights = previous_state.castling_rights;
        self.en_passant_square = previous_state.en_passant_square;
        self.halfmove_clock = previous_state.halfmove_clock;

        if self.side_to_move == PieceColour::Black {
            self.fullmove_number -= 1 // should only decrement if we're unmaking black's move
        }
    }

    pub fn update_internal_state_params(&mut self, move_made: Move) {
        // if move not capture & not pawn move, add 1 to halfmove clock
        if move_made.captured.is_none() & !(move_made.piece_moved == PieceKind::Pawn) {
            self.halfmove_clock += 1;
        }

        // castling rights lost if rook moves, king moves, rook captured
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
        let start_bitboard = 1u64 << move_to_make.start_square;
        let end_bitboard = 1u64 << move_to_make.end_square;

        let piece_moved = move_to_make.piece_moved;
        let colour_moved = self.side_to_move;

        let move_type = move_to_make.move_type;

        // remove piece at start
        self.bitboards.pieces[colour_moved as usize][piece_moved as usize] ^= start_bitboard;

        if let Some(captured) = move_to_make.captured
            && move_type == MoveType::Normal
        {
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
    }

    fn is_slider_in_view(
        &self,
        slider_kind: PieceKind,
        piece_colour: PieceColour,
        square_index: usize,
    ) -> bool {
        // test whether a slider can see the king by imagining the king is a slider of the same type, and testing if it sees the slider

        let sliders_to_check = self.bitboards.pieces[piece_colour as usize][slider_kind as usize];

        if sliders_to_check == 0 {
            return false;
        }; // don't bother with the rest if there are none on the board

        let trimmed_mask = match slider_kind {
            PieceKind::Bishop => TRIMMED_BISHOP_MASKS[square_index],
            PieceKind::Rook => TRIMMED_ROOK_MASKS[square_index],
            PieceKind::Queen => TRIMMED_QUEEN_MASKS[square_index],
            _ => unreachable!()
        };

        let blocker_config = self.bitboards.occupied & trimmed_mask;
        let attack_mask = lookup_magic(PieceKind::Queen, square_index, blocker_config);


        (sliders_to_check & attack_mask) != 0
    }

    fn update_sliders_in_view_of_square(
        &mut self,
        square: usize,
        mut recalculated: Recalculated,
    ) -> Recalculated {
        for slider_kind in SLIDERS {
            for colour in PIECE_COLOURS {
                if recalculated[colour as usize][slider_kind as usize] {
                    continue; // skip if already updated
                }

                if self.bitboards.pieces[colour as usize][slider_kind as usize] == 0 {
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
        for slider_kind in [PieceKind::Rook, PieceKind::Queen] {
            // bishops' attack masks never modified by castling

            if recalculated[colour as usize][slider_kind as usize] {
                continue;
            }

            if self.bitboards.pieces[colour as usize][slider_kind as usize] == 0 {
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
        let piece_moved = move_made.piece_moved;
        let side_to_move = self.side_to_move;
        let start_square = move_made.start_square;
        let end_square = move_made.end_square;

        // recompute only the attacked_from bitboard of the piece that's moved
        recalculated = self.conditional_attack_recalc(piece_moved, side_to_move, recalculated);

        if let Some(captured_piece) = move_made.captured {
            // same match as before, but with colours reversed
            recalculated =
                self.conditional_attack_recalc(captured_piece, side_to_move.flip(), recalculated)
        }

        // indirectly involved pieces (ie blocked/unblocked sliders)

        // for each colour and each slider type, check if there are any in view of the start/end square (without considering blocker)
        for square in [start_square, end_square] {
            recalculated = self.update_sliders_in_view_of_square(square, recalculated)
        }

        recalculated
    }

    fn update_aggregates_for_promotion(
        &mut self,
        promotion_choice: PieceKind,
        end_square: usize,
        recalculated: Recalculated,
    ) {
        use PieceKind as K;
        let side_to_move = self.side_to_move;

        match promotion_choice {
            K::Knight => {
                let new_mask = KNIGHT_ATTACKS[end_square];
                self.bitboards.attacked_by[side_to_move as usize][1] |= new_mask
            }
            K::Bishop | K::Rook | K::Queen => {
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
        use MoveType as T;
        use PieceColour as C;

        let affected_squares = match (side_to_move, castling_side) {
            (C::White, T::CastleKingside) => [4, 5, 6, 7],
            (C::White, T::CastleQueenside) => [0, 2, 3, 4],
            (C::Black, T::CastleKingside) => [60, 61, 62, 63],
            (C::Black, T::CastleQueenside) => [56, 58, 59, 60],
            _ => unreachable!("this function's only for castling"),
        };

        recalculated = self.conditional_attack_recalc(PieceKind::King, side_to_move, recalculated);
        recalculated = self.conditional_attack_recalc(PieceKind::Rook, side_to_move, recalculated);
        for square in affected_squares {
            self.update_same_side_sliders_in_view_of_square(square, recalculated);
            // opposing sliders don't need recalculation
        }
    }

    pub fn update_aggregates(&mut self, move_made: Move) {
        let mut recalculated = initialise_recalculated();
        let side_to_move = self.side_to_move;
        let start_square = move_made.start_square;
        let end_square = move_made.end_square;
        let move_type = move_made.move_type;

        match move_type {
            MoveType::Normal => {
                self.update_aggregates_for_normal_move(move_made, recalculated);
                if let Some(promotion_choice) = move_made.promotion {
                    self.update_aggregates_for_promotion(
                        promotion_choice,
                        end_square,
                        recalculated,
                    );
                }
            }
            MoveType::EnPassant => {
                // basically the same process as normal move, but we need to check if any sliders are unblocked on the captured piece square too
                recalculated = self.update_aggregates_for_normal_move(move_made, recalculated);
                let captured_pawn_square = (start_square & 0xF8) | (end_square & 0x7); // lovely bitop. start rank & end file
                self.update_sliders_in_view_of_square(captured_pawn_square, recalculated);
            }
            MoveType::CastleQueenside | MoveType::CastleKingside => {
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
        match piece_kind {
            PieceKind::Pawn => {
                self.bitboards.attacked_by[u_colour][0] = self.bitboards.pawn_attacks(colour);
            }
            PieceKind::Knight => {
                self.bitboards.attacked_by[u_colour][1] = self.bitboards.knight_attacks(colour)
            }
            PieceKind::Bishop => {
                self.bitboards.attacked_by[u_colour][2] = self.bitboards.slider_attacks(2, colour)
            }
            PieceKind::Rook => {
                self.bitboards.attacked_by[u_colour][3] = self.bitboards.slider_attacks(3, colour)
            }
            PieceKind::Queen => {
                self.bitboards.attacked_by[u_colour][4] = self.bitboards.slider_attacks(4, colour)
            }
            PieceKind::King => {
                self.bitboards.attacked_by[u_colour][5] = self.bitboards.king_attacks(colour)
            }
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
