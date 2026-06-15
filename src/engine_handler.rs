use crate::game_state::*;
use crate::mechanics::PieceColour;
use macroquad::prelude::*;
use std::time::{Duration, Instant};

use crate::evaluators::{NullEvaluator, PieceValues};
use crate::search_engines::{AlphaBetaPruning, Negamax, RandomSearch};

pub type Evaluation = i32;

pub enum BotVersion {
    Random,
    Negamax,
    AlphaBeta,
}

macro_rules! bot {
    ($search_engine: expr, $evaluator: expr) => {
        Bot {
            search_engine: Box::new($search_engine),
            evaluator: Box::new($evaluator),
        }
    };
}

impl BotVersion {
    pub fn to_bot(&self) -> Bot {
        use BotVersion as BV;
        match self {
            BV::Random => Bot {
                search_engine: Box::new(RandomSearch),
                evaluator: Box::new(NullEvaluator),
            },
            BV::Negamax => Bot {
                search_engine: Box::new(Negamax { depth: 4 }),
                evaluator: Box::new(PieceValues),
            },
            BV::AlphaBeta => bot!(AlphaBetaPruning { depth: 4 }, PieceValues),
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
        side_to_move: PieceColour,
        search_data: &mut SearchData,
        evaluator: &dyn Evaluator,
    ) -> Option<Move>;
}

pub trait Evaluator: Send + Sync {
    fn evaluate(
        &self,
        side_to_move: &PieceColour,
        search_state: &GameState,
        search_data: &mut SearchData,
    ) -> Evaluation;
}

impl Bot {
    pub fn create(argument: &str) -> Self {
        match argument {
            "random" => BotVersion::Random.to_bot(),
            "negamax" => BotVersion::Negamax.to_bot(),
            "alphabeta" => BotVersion::AlphaBeta.to_bot(),
            _ => panic!("Invalid bot selection"),
        }
    }

    pub fn choose_move(&self, game_state: &mut GameState) -> Option<Move> {
        let mut search_data = SearchData::new();
        let search_start_time = Instant::now();
        // remove the clone once it's all working
        let mut search_state = game_state.clone();

        if let Some(move_choice) = self.search_engine.search(
            &mut search_state,
            game_state.side_to_move,
            &mut search_data,
            &*self.evaluator,
        ) {
            search_data.time_taken = search_start_time.elapsed();
            search_data.log_search_results();
            return Some(move_choice);
        }

        None
    }
}

pub struct SearchData {
    pub current_eval: i32,
    pub nodes_evaluated: u64,
    pub time_taken: Duration,
}

impl SearchData {
    pub fn new() -> Self {
        SearchData {
            current_eval: 0,
            nodes_evaluated: 0,
            time_taken: Duration::new(0, 0),
        }
    }

    fn nps(&self) -> u64 {
        if self.time_taken.as_millis() == 0 {
            return 0u64;
        };
        1000 * self.nodes_evaluated / self.time_taken.as_millis() as u64
    }

    pub fn log_search_results(&self) {
        println!("-----------------------------------------------");
        println!("Eval: {}", self.current_eval);
        println!("Nodes evaluated: {}", self.nodes_evaluated);
        println!("Time taken: {}", self.time_taken.as_millis());
        println!("Nodes per second: {}", self.nps());
    }
}
