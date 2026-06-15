use crate::engine_handler::{Bot, Evaluation, Evaluator, SearchData, SearchEngine};
use crate::game_state::*;
use crate::mechanics::PieceColour;
use std::time::Instant;

pub struct NullEvaluator;
pub struct PieceValues;

impl Evaluator for NullEvaluator {
    fn evaluate(
        &self,
        _side_to_move: &PieceColour,
        _search_state: &GameState,
        _search_data: &mut SearchData,
    ) -> Evaluation {
        0
    }
}

impl Evaluator for PieceValues {
    fn evaluate(
        &self,
        side_to_move: &PieceColour,
        search_state: &GameState,
        _search_data: &mut SearchData,
    ) -> Evaluation {
        const PIECE_VALUES: [Evaluation; 5] = [100, 300, 301, 500, 900];

        let white_pieces = search_state.bitboards.pieces[0];
        let black_pieces = search_state.bitboards.pieces[1];

        let white_piece_value: Evaluation = (0usize..4usize)
            .map(|x| white_pieces[x].count_ones() as i32 * PIECE_VALUES[x])
            .sum();

        let black_piece_value: Evaluation = (0usize..4usize)
            .map(|x| black_pieces[x].count_ones() as i32 * PIECE_VALUES[x])
            .sum();

        match side_to_move {
            PieceColour::White => white_piece_value - black_piece_value,
            PieceColour::Black => black_piece_value - white_piece_value,
        }
    }
}
