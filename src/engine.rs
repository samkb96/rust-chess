use crate::mechanics::*;
use rand::rng;
use rand::seq::IteratorRandom;

pub fn bot_move(game_state: &mut GameState) -> Option<Move> {
    let legal_moves = game_state.legal_moves();
    let mut random_generator = rng();

    if let Some(&move_choice) = legal_moves.iter().choose(&mut random_generator) {
        game_state.make_move(move_choice);
        return Some(move_choice);
    }
    None
}
