use crate::game_state::*;
use crate::mechanics::{Piece, PieceColour};
use std::time::{Instant, Duration};
use macroquad::prelude::*;

type Evaluation = i32;

pub struct SearchData {
    current_eval: i32,
    nodes_evaluated: u64,
    time_taken: Duration
}

impl SearchData {
    fn new() -> Self {
        SearchData {
            current_eval: 0,
            nodes_evaluated: 0,
            time_taken: Duration::new(0, 0)
        }
    }

    fn nps(&self) -> u64 {
        if self.time_taken.as_millis() == 0 {
            return 0u64
        };
        1000 * (self.nodes_evaluated / self.time_taken.as_millis() as u64)
    }

    pub fn log_search_results(&self) {
        println!("-----------------------------------------------");
        println!("Eval: {}", self.current_eval);
        println!("Nodes evaluated: {}", self.nodes_evaluated);
        println!("Time taken: {}", self.time_taken.as_millis());
        println!("Nodes per second: {}", self.nps());
    }
}

pub fn bot_move(game_state: &mut GameState) -> Option<Move> {

    let mut search_data = SearchData::new();
    let search_start_time = Instant::now();
    // remove the clone once it's all working
    let mut search_state = game_state.clone();

    if let Some(move_choice) = search_moves(&mut search_state, game_state.side_to_move, 3, &mut search_data) {
        game_state.make_move(move_choice);
        search_data.time_taken = search_start_time.elapsed();
        search_data.log_search_results();
        return Some(move_choice)
    }

    None
}

fn evaluate(search_state: &GameState, side_to_move: PieceColour) -> Evaluation {
    // most basic evaluation, for now
    const PIECE_VALUES: [Evaluation; 5] = [100, 300, 301, 500, 900];

    let white_pieces = search_state.bitboards.pieces[0];
    let black_pieces = search_state.bitboards.pieces[1];

    let white_piece_value: Evaluation =
        (0usize..4usize)
            .map(
                |x| white_pieces[x].count_ones() as i32 * PIECE_VALUES[x]
            )
            .sum();

    let black_piece_value: Evaluation =
        (0usize..4usize)
            .map(
                |x| black_pieces[x].count_ones() as i32 * PIECE_VALUES[x]
            )
            .sum();

    match side_to_move {
        PieceColour::White => white_piece_value - black_piece_value,
        PieceColour::Black => black_piece_value - white_piece_value
    }
}

fn search_moves(search_state: &mut GameState, side_to_move: PieceColour, depth: u8, search_data: &mut SearchData) -> Option<Move> {

    let legal_moves = search_state.legal_moves();

    let mut best_score = i32::MIN;
    let mut best_move: Option<Move> = None;

    for mv in legal_moves {

        search_state.make_move(mv);

        let score = -negamax(search_state, side_to_move, depth.saturating_sub(1), search_data);
        search_state.unmake_move();

        if score > best_score {
            search_data.current_eval = score;
            best_score = score;
            best_move = Some(mv)
        }
    }

    best_move
}

fn negamax(search_state: &mut GameState, side_to_move: PieceColour, depth: u8, search_data: &mut SearchData) -> Evaluation {
    if depth == 0 {
        return evaluate(search_state, side_to_move)
    };

    let legal_moves = search_state.legal_moves();

    if legal_moves.is_empty() {
        return evaluate(search_state, side_to_move);
    };

    let mut best_score = i32::MIN;

    for mv in legal_moves {
        search_state.make_move(mv);
        search_data.nodes_evaluated += 1;
        let score = -negamax(search_state, side_to_move, depth - 1, search_data);
        search_state.unmake_move();
        best_score = best_score.max(score);
    }

    best_score
}
