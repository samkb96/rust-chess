use crate::engine::bot_handler::Bot;
use crate::game_state::{GameEnding, GameState};
use crate::constants::start_fens::TEST_POSITIONS_SUITE;
use libm::{erf, sqrt};
use std::cmp::Ordering;
use std::fmt::Display;

#[derive(Default)]
struct ArenaResult {
    first_bot_wins: u16,
    second_bot_wins: u16,
    draws: u16,
}

impl ArenaResult {
    fn record(&mut self, game_result: GameEnding, game_orientation: GameOrientation) {
        use GameWinner as W;

        match determine_winner(game_result, game_orientation) {
            Some(W::WhiteBot) => self.first_bot_wins += 1,
            Some(W::BlackBot) => self.second_bot_wins += 1,
            None => self.draws += 1,
        }
    }
}

impl Display for ArenaResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let first_bot_wins = self.first_bot_wins;
        let second_bot_wins = self.second_bot_wins;
        let draws = self.draws;

        let arena_winner = match (first_bot_wins - second_bot_wins).cmp(&0) {
            Ordering::Greater => "First bot",
            Ordering::Equal => "Neither",
            Ordering::Less => "Second bot",
        };

        // significance testing
        let los = if first_bot_wins + second_bot_wins == 0 {
            0.5
        } else {
            let numerator = first_bot_wins as f64 - second_bot_wins as f64;
            let denominator = sqrt((2 * first_bot_wins + 2 * second_bot_wins) as f64);
            0.5 * (1.0 + erf(numerator / denominator))
        };

        let significant = !(0.05..0.95).contains(&los);

        writeln!(f, "--- RESULTS ---")?;
        writeln!(f, "Leader: {arena_winner}")?;
        writeln!(f, "Record: {first_bot_wins}-{draws}-{second_bot_wins}")?;
        writeln!(
            f,
            "LOS: {:.1}%. {}",
            los * 100.0,
            if significant {
                "Significant imbalance"
            } else {
                "Not significant"
            }
        )
    }
}

pub fn bot_arena(white: &Bot, black: &Bot) {
    use GameOrientation as O;
    let mut arena_result = ArenaResult::default();

    println!("Commencing arena");

    for (idx, position_fen) in TEST_POSITIONS_SUITE.iter().enumerate() {
        if idx > 0 {
            println!("{arena_result}")
        }

        for game_orientation in [O::FirstAsWhite, O::FirstAsBlack] {
            let game_result = match game_orientation {
                O::FirstAsWhite => play_bot_game(position_fen, white, black),
                O::FirstAsBlack => play_bot_game(position_fen, black, white),
            };
            arena_result.record(game_result, game_orientation);
        }
    }
    println!("{arena_result}")
}

fn play_bot_game(position_fen: &str, white: &Bot, black: &Bot) -> GameEnding {
    let mut game_state = GameState::from_fen(position_fen);

    loop {
        if let Some(finished_after_white) =
            make_move_and_check_for_game_over(&mut game_state, white)
        {
            return finished_after_white;
        }
        if let Some(finished_after_black) =
            make_move_and_check_for_game_over(&mut game_state, black)
        {
            return finished_after_black;
        }
    }
}

fn make_move_and_check_for_game_over(game_state: &mut GameState, bot: &Bot) -> Option<GameEnding> {
    let search_state = game_state.clone();
    if let Some(move_to_make) = bot.choose_move(search_state) {
        game_state.make_move(move_to_make);
    } else {
        panic!("Should have been able to find a move")
    }

    let legal_moves = game_state.legal_moves();

    game_state.is_game_over(&legal_moves)
}

enum GameOrientation {
    FirstAsWhite,
    FirstAsBlack,
}

enum GameWinner {
    WhiteBot,
    BlackBot,
}

fn determine_winner(
    game_result: GameEnding,
    game_orientation: GameOrientation,
) -> Option<GameWinner> {
    use GameEnding as E;
    use GameOrientation as O;
    use GameWinner as W;

    match (game_result, game_orientation) {
        (E::WhiteWins, O::FirstAsWhite) | (E::BlackWins, O::FirstAsBlack) => Some(W::WhiteBot),
        (E::BlackWins, O::FirstAsWhite) | (E::WhiteWins, O::FirstAsBlack) => Some(W::BlackBot),
        _ => None,
    }
}
