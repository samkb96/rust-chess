use crate::game_state::*;
use macroquad::prelude::*;
use std::sync::Arc;
use std::time::{Duration, Instant};

use crate::engine::evaluators::{NullEvaluator, PieceSquareTables, PieceValues};
use crate::modes::mode_selection::GameModeError;
use crate::engine::search_engines::{AlphaBetaPruning, Negamax, RandomSearch};

pub type Evaluation = i32;

pub enum BotVersion {
    Random,
    Negamax,
    AlphaBeta,
    PieceSquareTable,
}

macro_rules! bot {
    ($search_engine: expr, $evaluator: expr) => {
        Arc::new(Bot {
            search_engine: Box::new($search_engine),
            evaluator: Box::new($evaluator),
        })
    };
}

impl BotVersion {
    pub fn to_bot(&self) -> Arc<Bot> {
        use BotVersion as BV;
        match self {
            BV::Random => bot!(RandomSearch, NullEvaluator),
            BV::Negamax => bot!(Negamax { depth: 3 }, PieceValues),
            BV::AlphaBeta => bot!(AlphaBetaPruning { depth: 4 }, PieceValues),
            BV::PieceSquareTable => bot!(AlphaBetaPruning { depth: 5 }, PieceSquareTables),
        }
    }
}

pub struct Bot {
    search_engine: Box<dyn SearchEngine>,
    evaluator: Box<dyn Evaluator>,
}

pub trait SearchEngine: Send + Sync {
    fn search(
        &self,
        search_state: &mut GameState,
        search_data: &mut SearchData,
        evaluator: &dyn Evaluator,
    ) -> Option<Move>;
}

pub trait Evaluator: Send + Sync {
    fn evaluate(&self, search_state: &GameState, search_data: &mut SearchData) -> Evaluation;
}

impl Bot {
    pub fn create(argument: &str) -> Result<Arc<Self>, GameModeError> {
        match argument {
            "random" => Ok(BotVersion::Random.to_bot()),
            "negamax" => Ok(BotVersion::Negamax.to_bot()),
            "alphabeta" => Ok(BotVersion::AlphaBeta.to_bot()),
            "pst" => Ok(BotVersion::PieceSquareTable.to_bot()),
            _ => Err(GameModeError::InvalidBotSelection(argument.to_string())),
        }
    }

    pub fn choose_move(&self, mut search_state: GameState) -> Option<Move> {
        let mut search_data = SearchData::default();
        let search_start_time = Instant::now();
        // remove the clone once it's all working

        if let Some(move_choice) =
            self.search_engine
                .search(&mut search_state, &mut search_data, &*self.evaluator)
        {
            search_data.time_taken = search_start_time.elapsed();
            //search_data.log_search_results();
            return Some(move_choice);
        }

        None
    }
}

#[derive(Default)]
pub struct SearchData {
    pub current_eval: i32,
    pub nodes_evaluated: u64,
    pub time_taken: Duration,
}

impl SearchData {
    fn nps(&self) -> u64 {
        if self.time_taken.as_millis() == 0 {
            return 0u64;
        };
        1000 * self.nodes_evaluated / self.time_taken.as_millis() as u64
    }

    pub fn log_search_results(&self) {
        println!("-----------------------------------------------");
        println!("Eval: {}", self.current_eval);
        println!(
            "Nodes evaluated: {}",
            thousands_separator(self.nodes_evaluated as usize)
        );
        println!(
            "Time taken: {}ms",
            thousands_separator(self.time_taken.as_millis() as usize)
        );
        println!(
            "Nodes per second: {}",
            thousands_separator(self.nps() as usize)
        );
    }
}

fn thousands_separator(n: usize) -> String {
    let s = n.to_string();
    let mut result = String::new();

    for (i, ch) in s.chars().rev().enumerate() {
        if i > 0 && i % 3 == 0 {
            result.insert(0, ',');
        }
        result.insert(0, ch);
    }
    result
}
