use crate::engine_handler::{Evaluation, Evaluator, SearchData, SearchEngine};
use crate::game_state::*;
use crate::mechanics::PieceColour;
use rand::prelude::*;
use std::ops::Neg;
use std::thread;

use std::time::Duration;

pub struct RandomSearch;
pub struct Negamax {
    pub depth: u8,
}
pub struct AlphaBetaPruning {
    pub depth: u8,
}
//
// -------------------------------------------------------- version 0 --------------------------------------------------------
//
impl SearchEngine for RandomSearch {
    fn search(
        &self,
        search_state: &mut GameState,
        _side_to_move: PieceColour,
        _search_data: &mut SearchData,
        _evaluator: &dyn Evaluator,
    ) -> Option<Move> {
        let legal_moves = search_state.legal_moves();
        let mut rng = rand::rng();
        thread::sleep(Duration::from_secs(1));
        legal_moves.choose(&mut rng).copied()
    }
}
//
// -------------------------------------------------------- version 1 --------------------------------------------------------
//
impl SearchEngine for Negamax {
    fn search(
        &self,
        search_state: &mut GameState,
        side_to_move: PieceColour,
        search_data: &mut SearchData,
        evaluator: &dyn Evaluator,
    ) -> Option<Move> {
        let legal_moves = search_state.legal_moves();
        let depth = self.depth;

        let mut best_score: Evaluation = i32::MIN;
        let mut best_move: Option<Move> = None;

        for mv in legal_moves {
            search_state.make_move(mv);
            let score = -Negamax::negamax(
                search_state,
                &side_to_move,
                depth.saturating_sub(1),
                search_data,
                evaluator,
            );
            search_state.unmake_move();

            if score > best_score {
                search_data.current_eval = score;
                best_score = score;
                best_move = Some(mv)
            }
        }
        best_move
    }
}

impl Negamax {
    fn negamax(
        search_state: &mut GameState,
        side_to_move: &PieceColour,
        depth: u8,
        search_data: &mut SearchData,
        evaluator: &dyn Evaluator,
    ) -> Evaluation {
        if depth == 0 {
            return evaluator.evaluate(side_to_move, search_state, search_data);
        }

        let legal_moves = search_state.legal_moves();

        if let Some(ending_eval) = evaluate_end_of_game(search_state, &legal_moves, side_to_move) {
            return ending_eval;
        }

        let mut best_score = i32::MIN;

        for mv in legal_moves {
            search_state.make_move(mv);
            search_data.nodes_evaluated += 1;
            let score = -Negamax::negamax(
                search_state,
                side_to_move,
                depth.saturating_sub(1),
                search_data,
                evaluator,
            );
            search_state.unmake_move();
            best_score = best_score.max(score);
        }
        best_score
    }
}
//
// -------------------------------------------------------- version 2 --------------------------------------------------------
//
impl SearchEngine for AlphaBetaPruning {
    fn search(
        &self,
        search_state: &mut GameState,
        side_to_move: PieceColour,
        search_data: &mut SearchData,
        evaluator: &dyn Evaluator,
    ) -> Option<Move> {
        let legal_moves = search_state.legal_moves();
        let depth = self.depth;

        let mut best_score: Evaluation = i32::MIN;
        let mut best_move: Option<Move> = None;

        let mut alpha: Evaluation = i32::MIN + 1;
        let mut beta: Evaluation = i32::MAX - 1;

        for mv in legal_moves {
            search_state.make_move(mv);
            let score = -AlphaBetaPruning::negamax(
                search_state,
                &side_to_move,
                depth.saturating_sub(1),
                search_data,
                evaluator,
                -beta,
                -alpha,
            );

            search_state.unmake_move();

            if score > best_score {
                search_data.current_eval = score;
                best_score = score;
                best_move = Some(mv)
            }

            if score >= beta {
                break;
            }

            alpha = best_score.max(alpha)
        }
        best_move
    }
}

impl AlphaBetaPruning {
    fn negamax(
        search_state: &mut GameState,
        side_to_move: &PieceColour,
        depth: u8,
        search_data: &mut SearchData,
        evaluator: &dyn Evaluator,
        mut alpha: Evaluation,
        mut beta: Evaluation,
    ) -> Evaluation {
        if depth == 0 {
            return evaluator.evaluate(side_to_move, search_state, search_data);
        }

        let legal_moves = search_state.legal_moves();

        if let Some(ending_eval) = evaluate_end_of_game(search_state, &legal_moves, side_to_move) {
            return ending_eval;
        }

        let mut best_score = i32::MIN;

        for mv in legal_moves {
            search_state.make_move(mv);
            search_data.nodes_evaluated += 1;

            let score = -AlphaBetaPruning::negamax(
                search_state,
                side_to_move,
                depth.saturating_sub(1),
                search_data,
                evaluator,
                beta.neg(),
                alpha.neg(),
            );

            search_state.unmake_move();
            best_score = best_score.max(score);
            if best_score > beta {
                break;
            }
            alpha = alpha.max(best_score);
        }

        best_score
    }
}

// misc helpers

fn evaluate_end_of_game(
    search_state: &GameState,
    legal_moves: &Moves,
    side_to_move: &PieceColour,
) -> Option<Evaluation> {
    if let Some(game_ending) = search_state.game_is_over(side_to_move, legal_moves) {
        use GameEnding as GE;
        use PieceColour as PC;
        match (side_to_move, game_ending) {
            (PC::White, GE::WhiteWins) | (PC::Black, GE::BlackWins) => Some(1000000),
            (PC::White, GE::BlackWins) | (PC::Black, GE::WhiteWins) => Some(-1000000),
            _ => Some(0), // draws
        }
    } else {
        None // not the end; keep evaluating
    }
}
