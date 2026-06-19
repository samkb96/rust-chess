use crate::engine::bot_handler::{Evaluation, Evaluator, SearchData};
use crate::constants::misc::psts::{BISHOP_PST, KING_PST, KNIGHT_PST, PAWN_PSTS, QUEEN_PST, ROOK_PST};
use crate::game_state::*;
use crate::mechanics::{PieceColour, pop_lsb};

pub struct NullEvaluator;
pub struct PieceValues;
pub struct PieceSquareTables;
//
// -------------------------------------------------------- version 0 --------------------------------------------------------
//
impl Evaluator for NullEvaluator {
    fn evaluate(&self, _search_state: &GameState, _search_data: &mut SearchData) -> Evaluation {
        0
    }
}
//
// -------------------------------------------------------- version 1 --------------------------------------------------------
//
impl Evaluator for PieceValues {
    fn evaluate(&self, search_state: &GameState, _search_data: &mut SearchData) -> Evaluation {
        let eval = piece_values(search_state);

        match search_state.side_to_move {
            PieceColour::White => eval,
            PieceColour::Black => -eval,
        }
    }
}

fn piece_values(search_state: &GameState) -> Evaluation {
    let white_pieces = search_state.bitboards.pieces[0];
    let black_pieces = search_state.bitboards.pieces[1];

    let white_piece_value: Evaluation = (0usize..=4usize)
        .map(|x| white_pieces[x].count_ones() as i32 * PIECE_VALUES[x])
        .sum();

    let black_piece_value: Evaluation = (0usize..=4usize)
        .map(|x| black_pieces[x].count_ones() as i32 * PIECE_VALUES[x])
        .sum();

    white_piece_value - black_piece_value
}

const PIECE_VALUES: [Evaluation; 5] = [100, 300, 301, 500, 900];
//
// -------------------------------------------------------- version 2 --------------------------------------------------------
//
impl Evaluator for PieceSquareTables {
    fn evaluate(&self, search_state: &GameState, _search_data: &mut SearchData) -> Evaluation {
        let piece_value_eval = piece_values(search_state);
        let piece_square_eval = get_piece_square_evals(search_state);
        let eval = piece_value_eval + piece_square_eval;

        match search_state.side_to_move {
            PieceColour::White => eval,
            PieceColour::Black => -eval,
        }
    }
}

fn get_piece_square_evals(search_state: &GameState) -> Evaluation {
    let mut score = 0;
    for piece_kind in 0..=5 {
        score += piece_square_eval(search_state, piece_kind, 0);
        score -= piece_square_eval(search_state, piece_kind, 1);
    }

    score
}

fn piece_square_eval(search_state: &GameState, piece: usize, colour: usize) -> Evaluation {
    let mut piece_bb = search_state.bitboards.pieces[colour][piece];

    let pst = match piece {
        0 => PAWN_PSTS[colour],
        1 => KNIGHT_PST,
        2 => BISHOP_PST,
        3 => ROOK_PST,
        4 => QUEEN_PST,
        5 => KING_PST,
        _ => unreachable!(),
    };

    let mut pst_score = 0;

    while let Some(lsb) = pop_lsb(&mut piece_bb) {
        pst_score += pst[lsb];
    }

    pst_score
}
