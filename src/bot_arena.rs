use crate::bot_handler::Bot;
use crate::game_state::{GameEnding, GameState};
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
    if let Some(move_to_make) = bot.choose_move(game_state) {
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

// opening suite is just a bunch of AI nonsense at the moment
// TODO write something to interface with lichess master database api
// maybe get evals from stockfish too
pub const TEST_POSITIONS_SUITE: [&str; 500] = [
    // Italian Game
    "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4",
    "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4",
    // Ruy Lopez
    "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3",
    "r1bqkb1r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 5 4",
    "r1bqk1nr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4",
    // French Defense
    "rnbqkb1r/ppp2ppp/3p1n2/8/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 1 4",
    "rnbqkb1r/ppp2ppp/3p1n2/8/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 2 4",
    "rnbqkb1r/ppp2ppp/3p1n2/8/3PP3/5N2/PPP2PPP/RNBQKB1R w KQkq - 1 4",
    // Caro-Kann
    "rnbqkbnr/ppp1pppp/8/3p4/3PP3/8/PPP2PPP/RNBQKBNR w KQkq d6 0 3",
    "rnbqkbnr/ppp1pppp/8/3p4/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 1 3",
    "rnbqkb1r/ppp1pppp/5n2/3p4/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 2 4",
    // Scandinavian
    "rnbqkbnr/ppp1pppp/8/3p4/3PP3/8/PPP2PPP/RNBQKBNR w KQkq d6 0 3",
    "rnbqkb1r/ppp1pppp/5n2/3p4/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 2 3",
    // Sicilian Defense
    "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2",
    "rnbqkbnr/pp1ppppp/8/2p5/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 2",
    "r1bqkbnr/pp1ppppp/2n5/2p1P3/8/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 3",
    "r1bqkb1r/pp1ppppp/2n2n2/2p1P3/8/2N5/PPPP1PPP/R1BQKBNR w KQkq - 2 4",
    // Queen's Gambit
    "rnbqkbnr/ppp1pppp/8/3p4/2PP4/8/PP2PPPP/RNBQKBNR b KQkq c3 0 2",
    "rnbqkbnr/ppp1pppp/8/3p4/2PP4/8/PP2PPPP/RNBQKBNR w KQkq - 0 3",
    "rnbqkbnr/ppp2ppp/8/3pp3/2PP4/8/PP2PPPP/RNBQKBNR w KQkq e6 0 3",
    "rnbqkb1r/ppp2ppp/5n2/3pp3/2PP4/8/PP2PPPP/RNBQKBNR w KQkq - 1 4",
    // King's Indian
    "rnbqk2r/ppppppbp/5np1/8/2PP4/8/PP2PPPP/RNBQKBNR w KQkq - 1 4",
    "rnbqk2r/ppppppbp/5np1/8/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 2 4",
    "rnbqk2r/ppppppbp/5np1/8/2PP4/5N2/PP2PPPP/RNBQKB1R w KQkq - 1 4",
    // Nimzo-Indian
    "rnbqk2r/ppppppbp/5np1/8/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 2 4",
    "r1bqk2r/ppppppbp/2n2np1/8/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 3 5",
    // English Opening
    "rnbqkbnr/pppppppp/8/8/2P5/8/PP1PPPPP/RNBQKBNR b KQkq c3 0 1",
    "rnbqkbnr/pppppppp/8/8/2P5/8/PP1PPPPP/RNBQKBNR w KQkq - 0 2",
    "rnbqkbnr/ppp1pppp/8/3p4/2P5/8/PP1PPPPP/RNBQKBNR w KQkq d6 0 2",
    // Réti Opening
    "rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 1 1",
    "rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R w KQkq - 0 2",
    "rnbqkbnr/ppp1pppp/8/3p4/8/5N2/PPPPPPPP/RNBQKB1R w KQkq d6 0 2",
    // Bird's Opening
    "rnbqkbnr/pppppppp/8/8/5P2/8/PPPPP1PP/RNBQKBNR b KQkq f3 0 1",
    // Alekhine's Defense
    "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
    "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2",
    "rnbqkbnr/pppp1ppp/4p3/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 3",
    "rnbqkb1r/pppp1ppp/4p1n1/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 1 3",
    // Two Knights
    "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4",
    // Evans Gambit
    "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P1P1/8/PPPP1P1P/RNBQK1NR b KQkq - 0 4",
    // Philidor Defense
    "rnbqkb1r/ppppp1pp/5n2/5p2/4P3/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3",
    // Pirc Defense
    "rnbqkb1r/ppp1pppp/3p1n2/8/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 1 4",
    // Modern Defense
    "rnbqkb1r/ppp1pppp/6n1/3p4/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 2 4",
    // Slav Defense
    "rnbqkb1r/pp2pppp/2np1n2/2p1P3/2PP4/2N5/PP3PPP/R1BQKBNR w KQkq - 1 5",
    // Semi-Slav
    "rnbqkb1r/pp2pppp/2np1n2/2p1P3/2PP4/2N2N2/PP3PPP/R1BQKB1R w KQkq - 2 5",
    // Grünfeld Defense
    "rnbqk2r/ppp1ppbp/3p1np1/8/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 1 5",
    // Benko Gambit
    "rnbqkbnr/p1pppppp/8/1p6/2PP4/8/PP2PPPP/RNBQKBNR w KQkq b6 0 2",
    // Orangutan Opening
    "rnbqkbnr/pppppppp/8/8/6P1/8/PPPPPP1P/RNBQKBNR b KQkq g3 0 1",
    // Italian Game variations
    "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4",
    "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4",
    "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R b KQkq - 1 4",
    "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R b KQkq - 1 4",
    // Ruy Lopez variations
    "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3",
    "r1bqkb1r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 5 4",
    "r1bqk1nr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4",
    "r2qkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 5 4",
    "r1bqk2r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 7 5",
    "r1bqk2r/pppp1ppp/2n2n2/1B2p3/4P3/3P1N2/PPP2PPP/RNBQ1RK1 b kq - 1 5",
    // French Defense variations
    "rnbqkb1r/ppp2ppp/3p1n2/8/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 1 4",
    "rnbqkb1r/ppp2ppp/3p1n2/8/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 2 4",
    "rnbqkb1r/ppp2ppp/3p1n2/8/3PP3/5N2/PPP2PPP/RNBQKB1R w KQkq - 1 4",
    "rnbqkb1r/ppp2ppp/3p1n2/8/3PP3/2N2N2/PPP2PPP/R1BQKB1R w KQkq - 3 4",
    "rnbqk2r/ppp2ppp/3p1n2/8/3PP3/2N2N2/PPP2PPP/R1BQKB1R b KQkq - 4 4",
    "rnbqk2r/ppp2ppp/3p1n2/8/3PP3/2N2NP1/PPP3PP/R1BQKB1R b KQkq - 0 4",
    "rnbqk1nr/ppp2ppp/3p1b2/8/3PP3/2N2N2/PPP2PPP/R1BQKB1R w KQkq - 4 4",
    // Caro-Kann variations
    "rnbqkbnr/ppp1pppp/8/3p4/3PP3/8/PPP2PPP/RNBQKBNR w KQkq d6 0 3",
    "rnbqkbnr/ppp1pppp/8/3p4/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 1 3",
    "rnbqkb1r/ppp1pppp/5n2/3p4/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 2 4",
    "rnbqkb1r/ppp1pppp/5n2/3p4/3PP3/5N2/PPP2PPP/RNBQKB1R w KQkq - 1 4",
    "rnbqk2r/ppp1ppbp/5np1/3p4/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 3 5",
    "rnbqk2r/ppp1ppbp/5np1/3p4/3PP3/5N2/PPP2PPP/RNBQKB1R w KQkq - 2 5",
    // Scandinavian variations
    "rnbqkb1r/ppp1pppp/5n2/3p4/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 2 3",
    "rnbqkb1r/ppp1pppp/5n2/3p4/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 3 3",
    "rnbqkb1r/ppp2ppp/5n2/3pp3/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 0 4",
    // Sicilian Defense variations
    "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2",
    "rnbqkbnr/pp1ppppp/8/2p5/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 2",
    "r1bqkbnr/pp1ppppp/2n5/2p1P3/8/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 3",
    "r1bqkb1r/pp1ppppp/2n2n2/2p1P3/8/2N5/PPPP1PPP/R1BQKBNR w KQkq - 2 4",
    "r1bqk1nr/pp1pppbp/2n5/2p1P3/8/2N5/PPPP1PPP/R1BQKBNR w KQkq - 3 4",
    "r1bqkb1r/pp2pppp/2np1n2/2p1P3/8/2N5/PPPP1PPP/R1BQKBNR w KQkq - 2 5",
    "r1bqkb1r/pp2pppp/2np1n2/2p1P3/2P5/2N5/PP2PPPP/R1BQKBNR b KQkq - 0 5",
    "r1bqkb1r/pp2pppp/2np1n2/2p1P3/2P5/2N2N2/PP2PPPP/R1BQKB1R b KQkq - 1 5",
    // Queen's Gambit variations
    "rnbqkbnr/ppp1pppp/8/3p4/2PP4/8/PP2PPPP/RNBQKBNR b KQkq c3 0 2",
    "rnbqkbnr/ppp2ppp/8/3pp3/2PP4/8/PP2PPPP/RNBQKBNR w KQkq e6 0 3",
    "rnbqkb1r/ppp2ppp/5n2/3pp3/2PP4/8/PP2PPPP/RNBQKBNR w KQkq - 1 4",
    "rnbqkb1r/ppp2ppp/5n2/3pp3/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 2 4",
    "rnbqkb1r/ppp3pp/5n2/3ppp2/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 1 5",
    "rnbqkb1r/pp3ppp/5n2/3ppp2/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 2 5",
    // King's Indian Attack
    "rnbqk2r/ppppppbp/5np1/8/2PP4/8/PP2PPPP/RNBQKBNR w KQkq - 1 4",
    "rnbqk2r/ppppppbp/5np1/8/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 2 4",
    "rnbqk2r/ppppppbp/5np1/8/2PP4/5N2/PP2PPPP/RNBQKB1R w KQkq - 1 4",
    "rnbqk2r/ppppppbp/5np1/8/2PP4/5NP1/PP2PP1P/RNBQKB1R b KQkq - 0 4",
    // Nimzo-Indian variations
    "rnbqk2r/ppppppbp/5np1/8/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 2 4",
    "r1bqk2r/ppppppbp/2n2np1/8/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 3 5",
    "r1bqk2r/ppppppbp/2n2np1/8/2PP4/2N2N2/PP2PPPP/R1BQK2R w KQkq - 4 5",
    // English Opening variations
    "rnbqkbnr/pppppppp/8/8/2P5/8/PP1PPPPP/RNBQKBNR b KQkq c3 0 1",
    "rnbqkbnr/ppp1pppp/8/3p4/2P5/8/PP1PPPPP/RNBQKBNR w KQkq d6 0 2",
    "rnbqkbnr/ppp1pppp/8/3p4/2P5/8/PP1PPPPP/RNBQKBNR w KQkq - 0 2",
    "rnbqkbnr/ppp1pppp/8/3p4/2P5/2N5/PP1PPPPP/R1BQKBNR w KQkq - 1 3",
    "rnbqkb1r/ppp1pppp/5n2/3p4/2P5/2N5/PP1PPPPP/R1BQKBNR w KQkq - 2 4",
    // Réti Opening variations
    "rnbqkbnr/pppppppp/8/8/8/5N2/PPPPPPPP/RNBQKB1R b KQkq - 1 1",
    "rnbqkbnr/ppp1pppp/8/3p4/8/5N2/PPPPPPPP/RNBQKB1R w KQkq d6 0 2",
    "rnbqkbnr/ppp1pppp/8/3p4/8/5N2/PPPPPPPP/RNBQKB1R w KQkq - 0 2",
    "rnbqkbnr/ppp1pppp/8/3p4/2P5/5N2/PP1PPPPP/RNBQKB1R b KQkq c3 0 2",
    "rnbqkb1r/ppp1pppp/5n2/3p4/2P5/5N2/PP1PPPPP/RNBQKB1R w KQkq - 1 4",
    // Bird's Opening variations
    "rnbqkbnr/pppppppp/8/8/5P2/8/PPPPP1PP/RNBQKBNR b KQkq f3 0 1",
    "rnbqkbnr/pppp1ppp/8/4p3/5P2/8/PPPPP1PP/RNBQKBNR w KQkq e6 0 2",
    // Alekhine's Defense variations
    "rnbqkbnr/pppp1ppp/4p3/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 3",
    "rnbqkbnr/pppp1ppp/4p3/8/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 3",
    "rnbqkb1r/pppp1ppp/4p1n1/8/4P3/2N5/PPPP1PPP/R1BQKBNR w KQkq - 2 4",
    "rnbqkb1r/pppp1ppp/4p1n1/8/4P3/2N2N2/PPPP1PPP/R1BQKB1R b KQkq - 3 4",
    // Two Knights Defense
    "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4",
    "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 5 5",
    // Evans Gambit
    "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P1P1/8/PPPP1P1P/RNBQK1NR b KQkq - 0 4",
    "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P1P1/5N2/PPPP1P1P/RNBQK2R b KQkq - 1 4",
    // Philidor Defense
    "rnbqkb1r/ppppp1pp/5n2/5p2/4P3/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3",
    "rnbqkb1r/ppppp1pp/5n2/5p2/4P3/2N5/PPPP1PPP/R1BQKBNR w KQkq - 1 4",
    // Pirc Defense variations
    "rnbqkb1r/ppp1pppp/3p1n2/8/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 1 4",
    "rnbqkb1r/ppp1pppp/3p1n2/8/3PP3/2N5/PPP2PPP/R1BQKBNR b KQkq - 2 4",
    "rnbqkb1r/ppp1pppp/3p1n2/8/3PP3/5N2/PPP2PPP/RNBQKB1R b KQkq - 1 4",
    // Modern Defense variations
    "rnbqkb1r/ppp1pppp/6n1/3p4/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 2 4",
    "rnbqkb1r/ppp1pppp/6n1/3p4/3PP3/2N5/PPP2PPP/R1BQKBNR b KQkq - 3 4",
    // Slav Defense variations
    "rnbqkb1r/pp2pppp/2np1n2/2p1P3/2PP4/2N5/PP3PPP/R1BQKBNR w KQkq - 1 5",
    "rnbqkb1r/pp2pppp/2np1n2/2p1P3/2PP4/2N2N2/PP3PPP/R1BQKB1R w KQkq - 2 5",
    "rnbqkb1r/pp3ppp/2npp1n1/2p1P3/2PP4/2N2N2/PP3PPP/R1BQKB1R w KQkq - 1 6",
    // Semi-Slav variations
    "rnbqkb1r/pp2pppp/2np1n2/2p1P3/2PP4/2N2N2/PP3PPP/R1BQKB1R w KQkq - 2 5",
    "rnbqk2r/pp2ppbp/2npp1n1/2p1P3/2PP4/2N2N2/PP3PPP/R1BQKB1R w KQkq - 2 6",
    // Grünfeld Defense variations
    "rnbqk2r/ppp1ppbp/3p1np1/8/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 1 5",
    "rnbqk2r/ppp1ppbp/3p1np1/8/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 2 5",
    "rnbqk2r/ppp1ppbp/3p1np1/8/2PPP3/2N5/PP4PP/R1BQKBNR b KQkq e3 0 5",
    // Benko Gambit
    "rnbqkbnr/p1pppppp/8/1p6/2PP4/8/PP2PPPP/RNBQKBNR w KQkq b6 0 2",
    "rnbqkbnr/p1pppppp/8/1p6/2PP4/2N5/PP2PPPP/R1BQKBNR b KQkq - 1 3",
    // Dutch Defense
    "rnbqkbnr/ppppp1pp/8/5p2/3P4/8/PPP1PPPP/RNBQKBNR w KQkq f6 0 2",
    "rnbqkbnr/ppppp1pp/8/5p2/3P4/5N2/PPP1PPPP/RNBQKB1R b KQkq - 1 3",
    // Orangutan Opening
    "rnbqkbnr/pppppppp/8/8/6P1/8/PPPPPP1P/RNBQKBNR b KQkq g3 0 1",
    "rnbqkbnr/pppp1ppp/8/4p3/6P1/8/PPPPPP1P/RNBQKBNR w KQkq e6 0 2",
    // King's Gambit variations
    "rnbqkbnr/pppppppp/8/8/4PP2/8/PPPP2PP/RNBQKBNR b KQkq e3 0 2",
    "rnbqkbnr/pppp1ppp/8/4p3/4PP2/8/PPPP2PP/RNBQKBNR w KQkq e6 0 3",
    "rnbqkbnr/pppp1ppp/8/4p3/4PP2/2N5/PPPP2PP/R1BQKBNR b KQkq - 1 3",
    "rnbqkb1r/pppp1ppp/5n2/4p3/4PP2/2N5/PPPP2PP/R1BQKBNR w KQkq - 2 4",
    "rnbqkb1r/pppp1ppp/5n2/4p3/4PPP1/8/PPPP3P/RNBQKBNR b KQkq - 0 4",
    "rnbqkb1r/pppp1ppp/5n2/4p3/4PPP1/5N2/PPPP3P/RNBQKB1R b KQkq - 1 4",
    "rnbqk2r/pppp1ppp/5n2/4p3/4PPP1/5N2/PPPP3P/RNBQKB1R w KQkq - 2 5",
    "rnbqk2r/pppp1ppp/5n2/4p3/2B1PPP1/5N2/PPPP3P/RNBQK2R b KQkq - 3 5",
    // Danish Gambit variations
    "rnbqkbnr/pppppppp/8/8/3PP3/8/PPP2PPP/RNBQKBNR b KQkq - 0 2",
    "rnbqkbnr/pppppppp/8/8/3PPP2/8/PPP3PP/RNBQKBNR b KQkq - 0 2",
    "rnbqkbnr/ppp1pppp/8/3p4/3PPP2/8/PPP3PP/RNBQKBNR w KQkq - 0 3",
    "rnbqkbnr/ppp1pppp/8/3p4/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 1 3",
    // Scotch Game variations
    "r1bqkbnr/pppppppp/2n5/8/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 3 2",
    "r1bqkbnr/pppppppp/2n5/8/3PP3/5N2/PPP2PPP/RNBQKB1R b KQkq - 0 3",
    "r1bqkb1r/pppppppp/2n2n2/8/3PP3/5N2/PPP2PPP/RNBQKB1R w KQkq - 2 4",
    "r1bqkb1r/pppppppp/2n2n2/8/3PPP2/5N2/PPP3PP/RNBQKB1R b KQkq - 0 4",
    "r1bqk2r/ppppppbp/2n2np1/8/3PPP2/5N2/PPP3PP/RNBQKB1R w KQkq - 2 5",
    // Giuoco Piano variations
    "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R b KQkq - 1 4",
    "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R w KQkq - 2 5",
    "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/2NP1N2/PPP2PPP/R1BQK2R b KQkq - 1 5",
    // Latvian Gambit
    "rnbqkbnr/pppp1ppp/8/4pp2/4P3/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 3",
    "rnbqkbnr/pppp1ppp/8/4pp2/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 3",
    "rnbqkb1r/pppp1ppp/5n2/4pp2/4P3/2N5/PPPP1PPP/R1BQKBNR w KQkq - 2 4",
    // Falkbeer Countergambit
    "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1",
    "rnbqkbnr/pppp1ppp/8/4pp2/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 3",
    "rnbqkbnr/pppp1ppp/8/4pp2/3PP3/8/PPP2PPP/RNBQKBNR b KQkq d3 0 3",
    // Center Game
    "rnbqkbnr/pppppppp/8/8/3PP3/8/PPP2PPP/RNBQKBNR b KQkq d3 0 2",
    "rnbqkbnr/pppppppp/8/8/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 0 2",
    "rnbqkbnr/ppp1pppp/8/3p4/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 0 3",
    "rnbqkbnr/ppp1pppp/8/3p4/3PPP2/8/PPP3PP/RNBQKBNR b KQkq - 0 3",
    // Göring Gambit
    "rnbqkbnr/ppp1pppp/8/3p4/3PPP2/2N5/PPP3PP/R1BQKBNR b KQkq - 1 3",
    "rnbqkbnr/ppp1pppp/8/3p4/3PPP2/2N5/PPP3PP/R1BQKBNR w KQkq - 0 3",
    "rnbqkb1r/ppp1pppp/5n2/3p4/3PPP2/2N5/PPP3PP/R1BQKBNR w KQkq - 1 4",
    // Vienna Game variations
    "rnbqkbnr/pppppppp/8/8/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 1",
    "rnbqkbnr/pppp1ppp/8/4p3/4P3/2N5/PPPP1PPP/R1BQKBNR w KQkq - 0 3",
    "rnbqkbnr/pppp1ppp/8/4p3/4P3/2N2N2/PPPP1PPP/R1BQKB1R b KQkq - 1 3",
    "rnbqkb1r/pppp1ppp/5n2/4p3/4P3/2N2N2/PPPP1PPP/R1BQKB1R w KQkq - 2 4",
    // Ruy Lopez Marshall Attack
    "r1bqk2r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 7 5",
    "r1bqk2r/pppp1ppp/2n2n2/1B2p3/4P3/3P1N2/PPP2PPP/RNBQ1RK1 b kq - 1 5",
    // Closed Sicilian
    "rnbqkbnr/pp1ppppp/8/2p5/2P5/8/PP1PPPPP/RNBQKBNR w KQkq - 0 2",
    "rnbqkbnr/pp1ppppp/8/2p5/2P5/2N5/PP1PPPPP/R1BQKBNR b KQkq - 1 2",
    "rnbqkbnr/pp1ppppp/8/2p5/2P5/2N2N2/PP1PPPPP/R1BQKB1R b KQkq - 2 2",
    "rnbqkb1r/pp1ppppp/5n2/2p5/2P5/2N2N2/PP1PPPPP/R1BQKB1R w KQkq - 3 4",
    // Grand Prix Attack
    "rnbqkbnr/pp1ppppp/8/2p5/4PP2/8/PPPP2PP/RNBQKBNR b KQkq e3 0 3",
    "rnbqkbnr/pp1ppppp/8/2p5/4PP2/2N5/PPPP2PP/R1BQKBNR b KQkq - 1 3",
    // 3.Bb5 Anti-Sicilian
    "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2",
    "rnbqkbnr/pp1ppppp/8/2p5/4P3/1B6/PPPP1PPP/RN1QKBNR b KQkq - 1 2",
    // Najdorf Sicilian
    "r1bqkb1r/pp2pppp/2np1n2/5p2/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 1 5",
    "r1bqkb1r/pp2pppp/2np1n2/5p2/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 0 5",
    "r1bqkb1r/pp2pppp/2np1n2/5p2/2PPP3/2N5/PP4PP/R1BQKBNR b KQkq - 0 5",
    // Sveshnikov Sicilian
    "r1bqkb1r/pp3ppp/2nppn2/8/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 1 5",
    "r1bqkb1r/pp3ppp/2nppn2/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 2 5",
    // Taimanov Sicilian
    "r1bqkb1r/pp2pppp/2nppn2/8/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 1 5",
    // Classical Sicilian
    "r1bqkbnr/pp1ppppp/2n5/2p1P3/8/2N5/PPPP1PPP/R1BQKBNR w KQkq - 1 4",
    "r1bqkb1r/pp1ppppp/2n2n2/2p1P3/8/2N5/PPPP1PPP/R1BQKBNR w KQkq - 2 4",
    // Lasker-Pelikan Sicilian
    "r1bqkb1r/pp3ppp/2np1n2/4p3/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 1 5",
    "r1bqkb1r/pp3ppp/2np1n2/4p3/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 2 5",
    // Accelerated Dragon
    "rnbqkb1r/pp2pppp/5n2/2p5/4P3/2N5/PPPP1PPP/R1BQKBNR w KQkq - 2 4",
    "rnbqkb1r/pp3ppp/5n2/2ppp3/4P3/2N5/PPPP1PPP/R1BQKBNR w KQkq - 0 5",
    "rnbqkb1r/pp3ppp/5n2/2ppp3/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 0 5",
    // Budapest Gambit
    "rnbqkbnr/pppp1ppp/8/4p3/2P5/8/PP1PPPPP/RNBQKBNR w KQkq - 0 3",
    "rnbqkbnr/pppp1ppp/8/4p3/2P5/5N2/PP1PPPPP/RNBQKB1R b KQkq - 1 3",
    "rnbqkb1r/pppp1ppp/5n2/4p3/2P5/5N2/PP1PPPPP/RNBQKB1R w KQkq - 2 4",
    // Albin Counter-Gambit
    "rnbqkbnr/ppp1pppp/8/3p4/2PP4/8/PP2PPPP/RNBQKBNR w KQkq - 0 3",
    "rnbqkbnr/ppp1pppp/8/3p4/2PPP3/8/PP3PPP/RNBQKBNR b KQkq e3 0 3",
    "rnbqkbnr/ppp2ppp/8/3pp3/2PPP3/8/PP3PPP/RNBQKBNR w KQkq - 0 4",
    // Tarrasch Defense
    "rnbqkb1r/pp2pppp/3p1n2/8/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 1 4",
    "rnbqkb1r/pp2pppp/3p1n2/8/3PP3/2N5/PPP2PPP/R1BQKBNR b KQkq - 2 4",
    "rnbqkb1r/pp2pppp/3p1n2/8/3PP3/2N2N2/PPP2PPP/R1BQKB1R b KQkq - 3 4",
    // Queen's Indian Defense
    "rnbqkb1r/pppp1ppp/4pn2/8/2PP4/8/PP2PPPP/RNBQKBNR w KQkq - 1 4",
    "rnbqkb1r/pppp1ppp/4pn2/8/2PP4/2N5/PP2PPPP/R1BQKBNR b KQkq - 2 4",
    "rnbqk2r/pppp1ppp/4pn2/8/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 3 5",
    // Old Indian Defense
    "rnbqkb1r/pppppppp/5n2/8/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 1 2",
    "rnbqkb1r/pppppppp/5n2/8/3P4/2N5/PPP1PPPP/R1BQKBNR b KQkq - 2 2",
    "rnbqk2r/ppppppbp/5n2/8/3P4/2N5/PPP1PPPP/R1BQKBNR w KQkq - 3 3",
    // Catalan Opening
    "rnbqkb1r/pppppppp/5n2/8/2PP4/6P1/PP2PP1P/RNBQKBNR b KQkq - 0 2",
    "rnbqkb1r/ppp1pppp/5n2/3p4/2PP4/6P1/PP2PP1P/RNBQKBNR w KQkq - 0 3",
    "rnbqkb1r/ppp1pppp/5n2/3p4/2PP4/6P1/PP2PPBP/RNBQK1NR b KQkq - 1 3",
    "rnbqk2r/ppp1ppbp/5n2/3p4/2PP4/6P1/PP2PPBP/RNBQK1NR w KQkq - 2 4",
    // Old Benoni
    "rnbqkbnr/pp1ppppp/8/2p5/3P4/8/PPP1PPPP/RNBQKBNR w KQkq c6 0 2",
    "rnbqkbnr/pp1ppppp/8/2p5/3P4/2N5/PPP1PPPP/R1BQKBNR b KQkq - 1 2",
    // Benoni Defense
    "rnbqkbnr/pp2pppp/8/2pp4/3P4/8/PPP1PPPP/RNBQKBNR w KQkq d6 0 3",
    "rnbqkbnr/pp2pppp/8/2pp4/3P4/2N5/PPP1PPPP/R1BQKBNR b KQkq - 1 3",
    "rnbqkb1r/pp2pppp/5n2/2pp4/3P4/2N5/PPP1PPPP/R1BQKBNR w KQkq - 2 4",
    // Blumenfeld Gambit
    "rnbqkbnr/pp1ppppp/8/2p5/1P1P4/8/P2BPPPP/RN1QKBNR b KQkq b3 0 3",
    // Fajarowicz Gambit
    "rnbqkbnr/pppp1ppp/8/4p3/1P6/8/P1PPPPPP/RNBQKBNR w KQkq - 0 3",
    // Sokolsky Opening (Orangutan)
    "rnbqkbnr/pppppppp/8/8/6P1/8/PPPPPP1P/RNBQKBNR b KQkq g3 0 1",
    "rnbqkbnr/pppp1ppp/8/4p3/6P1/8/PPPPPP1P/RNBQKBNR w KQkq e6 0 2",
    "rnbqkbnr/pppp1ppp/8/4p3/6P1/5N2/PPPPPP1P/RNBQKB1R b KQkq - 1 2",
    // Grob's Attack
    "rnbqkbnr/pppppppp/8/8/6P1/7P/PPPPPP2/RNBQKBNR b KQkq g3 0 1",
    // Polish Opening
    "rnbqkbnr/pppppppp/8/8/1P6/8/P1PPPPPP/RNBQKBNR b KQkq b3 0 1",
    "rnbqkbnr/pppp1ppp/8/4p3/1P6/8/P1PPPPPP/RNBQKBNR w KQkq - 0 2",
    // Ware Opening
    "rnbqkbnr/pppppppp/8/8/8/P7/1PPPPPPP/RNBQKBNR b KQkq - 0 1",
    // Van Geet Opening
    "rnbqkbnr/pppppppp/8/8/8/2N5/PPPPPPPP/R1BQKBNR b KQkq - 1 1",
    "rnbqkbnr/pppp1ppp/8/4p3/8/2N5/PPPPPPPP/R1BQKBNR w KQkq - 0 2",
    // Poisoned Pawn (Sicilian Najdorf)
    "r1bqkb1r/pp2pppp/2np1n2/5p2/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 0 5",
    "r1bqkb1r/pp2pppp/2np1n2/5p2/2PPP3/2N5/PP4PP/R1BQKBNR b KQkq - 0 5",
    // Hypermodern positions (various)
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
    "rnbqkbnr/pppppppp/8/8/3P4/8/PPP1PPPP/RNBQKBNR b KQkq d3 0 1",
    // Anti-Meran variations
    "rnbqkb1r/pp2pppp/3p1n2/2p1P3/2PP4/2N5/PP3PPP/R1BQKBNR w KQkq - 1 5",
    // Kampuchean Opening
    "rnbqkbnr/pppppppp/8/8/4P3/5P2/PPPP2PP/RNBQKBNR b KQkq f3 0 2",
    // Amar Opening
    "rnbqkbnr/pppppppp/8/8/8/7P/PPPPPPP1/RNBQKBNR b KQkq h3 0 1",
    // Marshall Attack (Ruy Lopez) - sharp tactics
    "r1bqk2r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 w kq - 5 5",
    "r1bqk2r/pppp1ppp/2n2n2/1B2p3/4P3/3P1N2/PPP2PPP/RNBQ1RK1 b kq - 1 5",
    "r1bqk1nr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4",
    "r1bq1rk1/pppp1ppp/2n2n2/1B2p3/4P3/3P1N2/PPP2PPP/RNBQ1RK1 w - - 6 6",
    "r1bq1rk1/pppp1ppp/2n2n2/1B2p3/3PP3/8/PPP2PPP/RNBQ1RK1 b - - 0 6",
    // Najdorf Sicilian - Black pawn storm
    "r1bqkb1r/pp2pppp/2np1n2/5p2/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 1 5",
    "r1bqk2r/pp2ppbp/2np1np1/5p2/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 1 6",
    "r1bqk2r/pp3pbp/2np1np1/2p1p3/2PP4/2N2N2/PP2PPPP/R1BQK2R w KQkq - 1 7",
    "r1bqk2r/pp3pbp/2np1np1/2p1P3/2PP4/2N5/PP2PPPP/R1BQK1NR b KQkq - 0 7",
    "r1bqk1nr/pp3pbp/2np2p1/2p1p3/2PP4/2N2N2/PP2PPPP/R1BQK2R w KQkq - 1 8",
    "r1bqk2r/pp3pbp/2np2p1/2p1P1p1/2PP4/2N5/PP2PPPP/R1BQK1NR w KQkq g6 0 8",
    "r1bq1rk1/pp3pbp/2np2p1/2p1p1p1/2PP4/2N5/PP2PPPP/R1BQK1NR w - - 0 9",
    // Sicilian Dragon - fianchettoed bishop
    "rnbqkb1r/pp2pppp/3p1n2/8/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 1 4",
    "rnbqkb1r/pp2pppp/3p1n2/8/3PP3/2N5/PPP2PPP/R1BQKBNR b KQkq - 2 4",
    "rnbqk2r/pp2ppbp/3p1np1/8/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 1 5",
    "rnbqk2r/pp2ppbp/3p1np1/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 2 5",
    "r1bqk2r/pp2ppbp/3p1np1/8/3PP3/2N2N2/PPP2PPP/R1BQK2R b KQkq - 3 5",
    "r1bqk2r/pp3pbp/3p1np1/2p1p3/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 0 6",
    "r1bqk2r/pp3pbp/3p1np1/2p1p3/3PP3/2N2NP1/PPP3PP/R1BQK2R b KQkq - 0 6",
    "r1bq1rk1/pp3pbp/3p1np1/2p1p3/3PP3/2N2NP1/PPP3PP/R1BQK2R w - - 1 7",
    // Grünfeld Defense - central complications
    "rnbqk2r/ppp1ppbp/3p1np1/8/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 1 5",
    "rnbqk2r/ppp1ppbp/3p1np1/8/2PP4/2N2N2/PP2PPPP/R1BQK2R w KQkq - 2 5",
    "r1bqk2r/ppp1ppbp/3p1np1/8/2PP4/2N2N2/PP2PPPP/R1BQK2R b KQkq - 3 5",
    "r1bqk2r/ppp1ppbp/3p1np1/8/2PPN3/2N5/PP2PPPP/R1BQK2R b KQkq - 0 6",
    "r1bqk2r/ppp1pp1p/3p1npb/8/2PPN3/2N5/PP2PPPP/R1BQK2R w KQkq - 1 7",
    "r1bqk2r/ppp1pp1p/3p1npb/8/2PPNP2/2N5/PP4PP/R1BQK2R b KQkq - 0 7",
    // King's Indian Attack - kingside pawn storm
    "rnbqk2r/ppppppbp/5np1/8/2PP4/5NP1/PP2PP1P/RNBQKB1R b KQkq - 1 4",
    "rnbqk2r/ppppppbp/5np1/8/2PP4/5NP1/PP2PP1P/RNBQKB1R w KQkq - 0 4",
    "rnbqk2r/pppp1pbp/5np1/4p3/2PP4/5NP1/PP2PP1P/RNBQKB1R w KQkq - 0 5",
    "rnbqk2r/pppp1pbp/5np1/4p3/2PP1P2/5NP1/PP4PP/RNBQKB1R b KQkq - 0 5",
    "rnbqk2r/pppp1pbp/5np1/4p3/2PP1P2/5NP1/PP3P1P/RNBQKBNR w KQkq - 0 6",
    "rnb1k2r/pppp1pbp/5np1/4p3/2PP1P2/5NP1/PP2BPBP/RN1QK2R b KQkq - 1 6",
    // Sicilian Sveshnikov - Black's counterattack
    "r1bqkb1r/pp2pppp/2nppn2/8/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 1 5",
    "r1bqkb1r/pp2pppp/2nppn2/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 2 5",
    "r1bqk2r/pp2ppbp/2nppnp1/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 6",
    "r1bqk2r/pp3pbp/2nppnp1/2p5/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 0 7",
    "r1bqk2r/pp3pbp/2nppnp1/2p5/3PP3/2N2NP1/PPP3PP/R1BQK2R b KQkq - 0 7",
    // French Defense - blocked center
    "rnbqkb1r/ppp2ppp/3p1n2/8/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 2 4",
    "rnbqkb1r/ppp2ppp/3p1n2/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 3 4",
    "rnbqk2r/ppp2pbp/3p1np1/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 5",
    "rnbqk2r/ppp2pbp/3p1np1/8/3PP3/2N2NP1/PPP3PP/R1BQK2R b KQkq - 0 5",
    "rnb1k2r/ppp2pbp/3p1np1/8/3PP3/2N2NP1/PPP3PP/R1BQK2R w KQkq - 1 6",
    // Caro-Kann - solid structure
    "rnbqkb1r/ppp1pppp/5n2/3p4/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 2 4",
    "rnbqkb1r/ppp1pppp/5n2/3p4/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 3 4",
    "rnbqk2r/ppp1ppbp/5np1/3p4/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 5",
    "rnbqk2r/ppp1ppbp/5np1/3p4/3PP3/2N2NP1/PPP3PP/R1BQK2R b KQkq - 0 5",
    "rnb1k2r/ppp1ppbp/5np1/3p4/3PP3/2N2NP1/PPP3PP/R1BQK2R w KQkq - 1 6",
    // Queen's Gambit Declined - structural tension
    "rnbqkb1r/ppp2ppp/5n2/3pp3/2PP4/2N5/PPP2PPP/R1BQKBNR w KQkq - 1 4",
    "rnbqkb1r/ppp2ppp/5n2/3pp3/2PP4/2N2N2/PPP2PPP/R1BQK2R w KQkq - 2 4",
    "rnbqk2r/ppp2pbp/5n2/3pp3/2PP4/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 5",
    "rnbqk2r/ppp3bp/5n2/3pp1p1/2PP4/2N2N2/PPP2PPP/R1BQK2R w KQkq - 0 6",
    "rnb1k2r/ppp3bp/5n2/3pp1p1/2PP4/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 7",
    // Catalan - bishop pair advantage
    "rnbqkb1r/ppp1pppp/5n2/3p4/2PP4/6P1/PP2PPBP/RNBQK1NR b KQkq - 1 3",
    "rnbqk2r/ppp1ppbp/5n2/3p4/2PP4/6P1/PP2PPBP/RNBQK1NR w KQkq - 2 4",
    "rnbqk2r/ppp1ppbp/5n2/3p4/2PP4/6P1/PP2PPBP/RNBQK1NR w KQkq - 0 4",
    "rnb1k2r/ppp1ppbp/5n2/3p4/2PP4/6P1/PP2PPBP/RNBQK1NR w KQkq - 1 5",
    // Benko Gambit - Black's queenside expansion
    "rnbqkbnr/p1pppppp/8/1p6/2PP4/8/PP2PPPP/RNBQKBNR w KQkq - 0 2",
    "rnbqkbnr/p1pppppp/8/1p6/2PP4/2N5/PP2PPPP/R1BQKBNR b KQkq - 1 2",
    "rnbqkb1r/p1pppppp/5n2/1p6/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 2 3",
    "rnbqkb1r/p1pppppp/5n2/1p6/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 3 4",
    "rnb1kb1r/p1pppppp/5n2/1p6/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 4 5",
    // Ruy Lopez Closed - maneuvering play
    "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4",
    "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/3P1N2/PPP2PPP/RNBQK2R b KQkq - 1 4",
    "r1bqk1nr/pppp1ppp/2n5/1B2p3/4P3/3P1N2/PPP2PPP/RNBQK2R w KQkq - 2 5",
    "r1bqk2r/pppp1ppp/2n2n2/1B2p3/4P3/3P1N2/PPP2PPP/RNBQK2R w KQkq - 3 6",
    "r1bqk2r/pppp1ppp/2n2n2/1B2p3/3PP3/8/PPP2PPP/RNBQK1NR b KQkq - 0 6",
    // Slav Defense - central control
    "rnbqkb1r/pp2pppp/2np1n2/2p1P3/2PP4/2N2N2/PP3PPP/R1BQKB1R w KQkq - 1 6",
    "rnbqkb1r/pp3ppp/2np1n2/2p1p3/2PP4/2N2N2/PP3PPP/R1BQKB1R w KQkq - 0 7",
    "rnbqk2r/pp3pbp/2npp1p1/2p5/2PP4/2N2N2/PP3PPP/R1BQKB1R w KQkq - 1 8",
    // Semi-Slav - complex middlegame
    "rnbqk2r/pp2ppbp/2npp1p1/2p5/2PP4/2N2N2/PP3PPP/R1BQKB1R w KQkq - 1 8",
    "rnbqk2r/pp3pbp/2npp1p1/2p5/2PP4/2N2NP1/PP4PP/R1BQKB1R b KQkq - 0 8",
    // King's Indian - fianchettoed kingside
    "rnbqk2r/ppppppbp/5np1/8/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 1 4",
    "rnbqk2r/ppppppbp/5np1/8/2PP4/2N2N2/PP2PPPP/R1BQK2R w KQkq - 2 5",
    "rnbqk2r/pppp1pbp/5np1/4p3/2PP4/2N2N2/PP2PPPP/R1BQK2R w KQkq - 0 6",
    "rnb1k2r/pppp1pbp/5np1/4p3/2PP1P2/2N2N2/PP4PP/R1BQKB1R w KQkq - 1 7",
    // Nimzo-Indian - positional struggle
    "r1bqk2r/ppppppbp/2n2np1/8/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 1 5",
    "r1bqk2r/ppppppbp/2n2np1/8/2PP4/2N2N2/PP2PPPP/R1BQK2R w KQkq - 2 5",
    "r1bqk2r/pppp1pbp/2n2np1/4p3/2PP4/2N2N2/PP2PPPP/R1BQK2R w KQkq - 0 6",
    // English Opening - flexible pawn structure
    "rnbqkbnr/ppp1pppp/8/3p4/2P5/8/PP1PPPPP/RNBQKBNR w KQkq - 0 2",
    "rnbqkbnr/ppp1pppp/8/3p4/2P5/2N5/PP1PPPPP/R1BQKBNR b KQkq - 1 2",
    "rnbqkb1r/ppp1pppp/5n2/3p4/2P5/2N5/PP1PPPPP/R1BQKBNR w KQkq - 2 3",
    "rnbqkb1r/ppp1pppp/5n2/3p4/2P5/2N2N2/PP1PPPPP/R1BQK2R w KQkq - 3 4",
    "rnb1kb1r/ppp1pppp/5n2/3p4/2P5/2N2N2/PP1PPPPP/R1BQK2R w KQkq - 4 5",
    // Réti Opening - hypermodern ideas
    "rnbqkbnr/ppp1pppp/8/3p4/8/5N2/PPPPPPPP/RNBQKB1R w KQkq - 0 2",
    "rnbqkbnr/ppp1pppp/8/3p4/2P5/5N2/PP1PPPPP/RNBQKB1R b KQkq - 0 2",
    "rnbqkb1r/ppp1pppp/5n2/3p4/2P5/5N2/PP1PPPPP/RNBQKB1R w KQkq - 1 4",
    "rnbqkb1r/ppp1pppp/5n2/3p4/2P5/2N2N2/PP1PPPPP/R1BQK2R w KQkq - 2 5",
    // Sicilian Taimanov - tactical middlegame
    "r1bqkb1r/pp2pppp/2nppn2/8/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 1 5",
    "r1bqkb1r/pp2pppp/2nppn2/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 2 5",
    "r1bqk2r/pp2ppbp/2nppnp1/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 6",
    // Sicilian Classical - maneuvering
    "r1bqkbnr/pp1ppppp/2n5/2p1P3/8/2N5/PPPP1PPP/R1BQKBNR w KQkq - 1 4",
    "r1bqkb1r/pp1ppppp/2n2n2/2p1P3/8/2N5/PPPP1PPP/R1BQKBNR w KQkq - 2 4",
    "r1bqkb1r/pp1ppppp/2n2n2/2p1P3/3P4/2N5/PPP2PPP/R1BQKBNR b KQkq - 0 5",
    // Two Knights Defense - sharp tactics
    "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R b KQkq - 1 4",
    "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R w KQkq - 2 5",
    // Giuoco Piano - quiet but sharp
    "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R w KQkq - 4 4",
    "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R w KQkq - 5 5",
    // Pirc Defense - active piece play
    "rnbqkb1r/ppp1pppp/3p1n2/8/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 1 4",
    "rnbqkb1r/ppp1pppp/3p1n2/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 2 4",
    "rnb1kb1r/ppp1pppp/3p1n2/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 3 5",
    // Modern Defense - positional flexibility
    "rnbqkb1r/ppp1pppp/6n1/3p4/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 2 4",
    "rnbqkb1r/ppp1pppp/6n1/3p4/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 3 4",
    // Vienna Game - quiet center
    "rnbqkbnr/pppp1ppp/8/4p3/4P3/2N2N2/PPPP1PPP/R1BQKB1R b KQkq - 1 3",
    "rnbqkb1r/pppp1ppp/5n2/4p3/4P3/2N2N2/PPPP1PPP/R1BQKB1R w KQkq - 2 4",
    // Philidor Defense - solid setup
    "rnbqkb1r/ppppp1pp/5n2/5p2/4P3/2N5/PPPP1PPP/R1BQKBNR w KQkq - 2 4",
    "rnbqkb1r/ppppp1pp/5n2/5p2/4P3/2N2N2/PPPP1PPP/R1BQK2R w KQkq - 3 4",
    // Scandinavian Defense - tactical chances
    "rnbqkb1r/ppp1pppp/5n2/3p4/3PP3/2N5/PPP2PPP/R1BQKBNR w KQkq - 2 4",
    "rnbqkb1r/ppp1pppp/5n2/3p4/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 3 4",
    // Sharp Sicilian Najdorf positions
    "r1bqkb1r/pp2pppp/2np1n2/5p2/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 0 5",
    "r1bqk2r/pp2ppbp/2np1np1/5p2/2PP4/2N2N2/PP2PPPP/R1BQK2R w KQkq - 1 6",
    "r1bqk2r/pp3pbp/2np1np1/2p1p3/2PP4/2N2N2/PP2PPPP/R1BQK2R w KQkq - 1 7",
    "r1bqk2r/pp3pbp/2np2p1/2p1p1p1/2PP4/2N2N2/PP2PPPP/R1BQK2R w KQkq - 1 8",
    "r1bq1rk1/pp3pbp/2np2p1/2p1p1p1/2PP4/2N2N2/PP2PPPP/R1BQK2R w - - 2 9",
    // King's Gambit Accepted - forcing play
    "rnbqkbnr/pppp1ppp/8/4pp2/4PP2/8/PPPP2PP/RNBQKBNR w KQkq e6 0 3",
    "rnbqkbnr/pppp1ppp/8/4pp2/4PP2/2N5/PPPP2PP/R1BQKBNR b KQkq - 1 3",
    "rnbqkb1r/pppp1ppp/5n2/4pp2/4PP2/2N5/PPPP2PP/R1BQKBNR w KQkq - 2 4",
    "rnbqkb1r/pppp1ppp/5n2/4pp2/3PPP2/2N5/PPP3PP/R1BQKBNR b KQkq - 0 4",
    "rnbqkb1r/pppp1ppp/5n2/4pp2/3PPP2/2N5/PPP3PP/R1BQK1NR w KQkq - 1 5",
    // Sharp Sicilian Dragon positions
    "rnbqk2r/pp2ppbp/3p1np1/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 5",
    "rnbqk2r/pp2ppbp/3p1np1/8/3PP3/2N2NP1/PPP3PP/R1BQK2R b KQkq - 0 5",
    "rnb1k2r/pp2ppbp/3p1np1/8/3PP3/2N2NP1/PPP3PP/R1BQK2R w KQkq - 1 6",
    "rnb1k2r/pp3pbp/3p1np1/2p5/3PP3/2N2NP1/PPP3PP/R1BQK2R w KQkq - 0 7",
    "rnb1k2r/pp3pbp/3p1np1/2p5/3PP3/2N2NPP/PPP4P/R1BQK2R b KQkq - 0 7",
    // Sicilian Sveshnikov - Black counterplay
    "r1bqkb1r/pp2pppp/2nppn2/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 2 5",
    "r1bqk2r/pp2ppbp/2nppnp1/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 6",
    "r1bqk2r/pp3pbp/2nppnp1/2p5/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 0 7",
    "r1bqk2r/pp3pbp/2nppnp1/2p5/3PP3/2N2NP1/PPP3PP/R1BQK2R b KQkq - 0 7",
    // Marshall Attack (Ruy Lopez) - sharp tactics
    "r1bqk2r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 w kq - 5 5",
    "r1bqk2r/pppp1ppp/2n2n2/1B2p3/4P3/3P1N2/PPP2PPP/RNBQ1RK1 b kq - 1 5",
    "r1bqk1nr/pppp1ppp/2n5/1B2p3/4P3/3P1N2/PPP2PPP/RNBQK2R b KQkq - 2 5",
    "r1bq1rk1/pppp1ppp/2n2n2/1B2p3/4P3/3P1N2/PPP2PPP/RNBQ1RK1 w - - 6 6",
    // Grünfeld Defense - sharp center
    "rnbqk2r/ppp1ppbp/3p1np1/8/2PP4/2N2N2/PP2PPPP/R1BQK2R w KQkq - 2 5",
    "rnbqk2r/ppp1ppbp/3p1np1/8/2PP4/2N2NP1/PP2PP1P/R1BQK2R b KQkq - 0 5",
    "rnb1k2r/ppp1ppbp/3p1np1/8/2PP4/2N2NP1/PP2PP1P/R1BQK2R w KQkq - 1 6",
    "rnb1k2r/ppp1ppbp/3p1np1/8/2PPP3/2N2NP1/PP4PP/R1BQK2R b KQkq - 0 6",
    // Sicilian Classical - tactical middlegame
    "r1bqkbnr/pp1ppppp/2n5/2p1P3/3P4/2N5/PPP2PPP/R1BQKBNR b KQkq - 1 5",
    "r1bqkb1r/pp1ppppp/2n2n2/2p1P3/3P4/2N5/PPP2PPP/R1BQKBNR w KQkq - 2 5",
    "r1bqkb1r/pp2pppp/2nppn2/2p1P3/3P4/2N5/PPP2PPP/R1BQKBNR w KQkq - 1 6",
    "r1bqk2r/pp2ppbp/2nppnp1/2p1P3/3P4/2N5/PPP2PPP/R1BQKBNR w KQkq - 1 7",
    // Sicilian Taimanov - tactical play
    "r1bqkb1r/pp2pppp/2nppn2/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 5",
    "r1bqk2r/pp2ppbp/2nppnp1/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 6",
    "r1bqk2r/pp2ppbp/2nppnp1/8/3PP3/2N2NP1/PPP3PP/R1BQK2R b KQkq - 0 6",
    // Two Knights Defense - forcing variations
    "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R b KQkq - 1 4",
    "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R w KQkq - 2 5",
    "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/2NP1N2/PPP2PPP/R1BQK2R b KQkq - 1 5",
    "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/2NP1N2/PPP2PPP/R1BQK2R w KQkq - 2 6",
    // French Defense - sharp variations
    "rnbqkb1r/ppp2ppp/3p1n2/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 3 4",
    "rnbqk2r/ppp2pbp/3p1np1/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 5",
    "rnbqk2r/ppp2pbp/3p1np1/8/3PP3/2N2NP1/PPP3PP/R1BQK2R b KQkq - 0 5",
    "rnb1k2r/ppp2pbp/3p1np1/8/3PP3/2N2NP1/PPP3PP/R1BQK2R w KQkq - 1 6",
    // Caro-Kann - sharp lines
    "rnbqkb1r/ppp1pppp/5n2/3p4/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 3 4",
    "rnbqk2r/ppp1ppbp/5n2/3p4/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 5",
    "rnbqk2r/ppp1ppbp/5n2/3p4/3PP3/2N2NP1/PPP3PP/R1BQK2R b KQkq - 0 5",
    // Alekhine's Defense - tactical complications
    "rnbqkb1r/pppp1ppp/4p1n1/8/4P3/2N2N2/PPPP1PPP/R1BQKB1R w KQkq - 2 4",
    "rnbqk2r/pppp1pbp/4p1n1/8/4P3/2N2N2/PPPP1PPP/R1BQK2R w KQkq - 3 5",
    "rnbqk2r/pppp1pbp/4p1n1/8/4P3/2N2NP1/PPPP1P1P/R1BQK2R b KQkq - 0 5",
    // Scandinavian Defense - open positions
    "rnbqkb1r/ppp1pppp/5n2/3p4/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 2 4",
    "rnbqk2r/ppp1ppbp/5n2/3p4/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 5",
    "rnbqk2r/ppp1ppbp/5n2/3p4/3PP3/2N2NP1/PPP3PP/R1BQK2R b KQkq - 0 5",
    // Ruy Lopez Open - sharp tactics
    "r1bqk1nr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4",
    "r1bqk2r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 5 5",
    "r1bqk1nr/pppp1ppp/2n5/1B2p3/4P3/3P1N2/PPP2PPP/RNBQK2R b KQkq - 1 5",
    // Giuoco Piano - sharp positions
    "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R w KQkq - 1 4",
    "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/3P1N2/PPP2PPP/RNBQK2R w KQkq - 2 5",
    "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P3/2NP1N2/PPP2PPP/R1BQK2R b KQkq - 1 5",
    // Sicilian Accelerated Dragon - complications
    "rnbqkb1r/pp2pppp/5n2/2p5/4P3/2N2N2/PPPP1PPP/R1BQKB1R w KQkq - 2 4",
    "rnbqkb1r/pp2pppp/5n2/2p5/4P3/2N2NP1/PPPP1P1P/R1BQKB1R b KQkq - 0 4",
    "rnbqkb1r/pp3ppp/5n2/2ppp3/4P3/2N2NP1/PPPP1P1P/R1BQKB1R w KQkq - 0 5",
    "rnbqkb1r/pp3ppp/5n2/2ppp3/3PP3/2N2NP1/PPP3PP/R1BQKB1R b KQkq - 0 5",
    // Vienna Game - tactical openings
    "rnbqkbnr/pppp1ppp/8/4p3/4P3/2N2N2/PPPP1PPP/R1BQKB1R b KQkq - 1 3",
    "rnbqkb1r/pppp1ppp/5n2/4p3/4P3/2N2N2/PPPP1PPP/R1BQKB1R w KQkq - 2 4",
    "rnbqk2r/pppp1pbp/5n2/4p3/4P3/2N2N2/PPPP1PPP/R1BQK2R w KQkq - 3 5",
    // Philidor Defense - tactical chances
    "rnbqkb1r/ppppp1pp/5n2/5p2/4P3/2N2N2/PPPP1PPP/R1BQKB1R w KQkq - 2 4",
    "rnbqk2r/ppppp1bp/5n2/5p2/4P3/2N2N2/PPPP1PPP/R1BQK2R w KQkq - 3 5",
    // Pirc Defense - sharp continuations
    "rnbqkb1r/ppp1pppp/3p1n2/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 4",
    "rnbqk2r/ppp1ppbp/3p1np1/8/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 5",
    // Modern Defense - tactical patterns
    "rnbqkb1r/ppp1pppp/6n1/3p4/3PP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 4",
    "rnbqk2r/ppp1ppbp/6n1/3p4/3PP3/2N2NP1/PPP3PP/R1BQK2R b KQkq - 0 4",
    // Evans Gambit - wild tactics
    "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P1P1/5N2/PPPP1P1P/RNBQK2R b KQkq - 1 4",
    "r1bqk2r/pppp1ppp/2n2n2/2b1p3/2B1P1P1/5N2/PPPP1P1P/RNBQK2R w KQkq - 2 5",
    // Latvian Gambit - sharp play
    "rnbqkbnr/pppp1ppp/8/4pp2/4P3/2N5/PPPP1PPP/R1BQKBNR b KQkq - 1 3",
    "rnbqkb1r/pppp1ppp/5n2/4pp2/4P3/2N5/PPPP1PPP/R1BQKBNR w KQkq - 2 4",
    // King's Gambit Declined - solid but sharp
    "rnbqkbnr/pppp1ppp/8/4p3/4PP2/8/PPPP2PP/RNBQKBNR w KQkq - 0 3",
    "rnbqkbnr/pppp1ppp/8/4p3/4PP2/2N5/PPPP2PP/R1BQKBNR b KQkq - 1 3",
    // Benko Gambit - queenside complications
    "rnbqkb1r/p1pppppp/5n2/1p6/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 2 4",
    "rnb1kb1r/p1pppppp/5n2/1p6/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 3 5",
    // Semi-Slav - sharp continuations
    "rnbqk2r/pp2ppbp/2npp1p1/2p5/2PP4/2N2N2/PP3PPP/R1BQKB1R w KQkq - 1 8",
    "rnbqk2r/pp3pbp/2npp1p1/2p5/2PP4/2N2NP1/PP4PP/R1BQKB1R b KQkq - 0 8",
    // Slav Defense - tactical positions
    "rnbqk2r/pp2ppbp/2npp1p1/2p5/2PP4/2N2N2/PP3PPP/R1BQKB1R w KQkq - 1 8",
    "rnbqk2r/pp3pbp/2npp1p1/2p5/2PP4/2N2NP1/PP4PP/R1BQKB1R b KQkq - 0 8",
    // Queen's Gambit Accepted - sharp lines
    "rnbqkbnr/ppp1pppp/8/8/2PP4/2N5/PP2PPPP/R1BQKBNR b KQkq - 1 2",
    "rnbqkb1r/ppp1pppp/5n2/8/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 2 3",
    "rnbqkb1r/ppp1pppp/5n2/8/2PP4/2N2N2/PP2PPPP/R1BQK2R w KQkq - 3 4",
    // Scotch Game - open positions
    "r1bqkb1r/pppppppp/2n2n2/8/3PP3/5N2/PPP2PPP/RNBQKB1R w KQkq - 2 4",
    "r1bqkb1r/pppppppp/2n2n2/8/3PPP2/5N2/PPP3PP/RNBQKB1R b KQkq - 0 4",
    // Ruy Lopez Steinitz - tactical middlegame
    "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 4 4",
    "r1bqkb1r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 5 5",
    // Queen's Indian - active piece play
    "rnbqkb1r/pppp1ppp/4pn2/8/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 1 4",
    "rnbqk2r/pppp1pbp/4pn2/8/2PP4/2N5/PP2PPPP/R1BQKBNR w KQkq - 2 5",
    // Catalan - sharp bishop pair play
    "rnbqkb1r/ppp1pppp/5n2/3p4/2PP4/6P1/PP2PPBP/RNBQK1NR b KQkq - 1 3",
    "rnbqk2r/ppp1ppbp/5n2/3p4/2PP4/6P1/PP2PPBP/RNBQK1NR w KQkq - 2 4",
    "rnbqk2r/ppp1ppbp/5n2/3p4/2PP4/6P1/PP2PPBP/RNBQK1NR w KQkq - 0 4",
    // English Opening - flexible tactics
    "rnbqkb1r/ppp1pppp/5n2/3p4/2P5/2N2N2/PP1PPPPP/R1BQKB1R w KQkq - 2 4",
    "rnbqkb1r/ppp1pppp/5n2/3p4/2P5/2N2NP1/PP1PPPBP/R1BQK2R b KQkq - 1 4",
    // Réti Opening - hypermodern tactics
    "rnbqkb1r/ppp1pppp/5n2/3p4/2P5/5N2/PP1PPPPP/RNBQKB1R w KQkq - 1 3",
    "rnbqkb1r/ppp1pppp/5n2/3p4/2P5/2N2N2/PP1PPPPP/R1BQKB1R w KQkq - 2 4",
    // Closed Sicilian - positional sharpness
    "rnbqkbnr/pp1ppppp/8/2p5/2P5/2N2N2/PP1PPPPP/R1BQKB1R b KQkq - 2 2",
    "rnbqkb1r/pp1ppppp/5n2/2p5/2P5/2N2N2/PP1PPPPP/R1BQKB1R w KQkq - 3 4",
    // Grand Prix Attack - aggressive setup
    "rnbqkbnr/pp1ppppp/8/2p5/4PP2/8/PPPP2PP/RNBQKBNR b KQkq e3 0 3",
    "rnbqkbnr/pp1ppppp/8/2p5/4PP2/2N5/PPPP2PP/R1BQKBNR b KQkq - 1 3",
    // Falkbeer Countergambit - dynamic play
    "rnbqkbnr/pppp1ppp/8/4pp2/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 3",
    "rnbqkbnr/pppp1ppp/8/4pp2/3PP3/8/PPP2PPP/RNBQKBNR b KQkq d3 0 3",
    // Ruy Lopez - maneuvering with space advantage
    "r1bqk2r/pppp1ppp/2n2n2/1B2p3/4P3/3P1N2/PPP2PPP/RNBQ1RK1 b kq - 1 6",
    "r1bqk2r/pppp1ppp/2n2n2/1B2p3/3PP3/8/PPP2PPP/RNBQ1RK1 b kq - 0 6",
    "r1b1k2r/pppp1ppp/2n2n2/1B2p3/3PP3/8/PPP2PPP/RNBQ1RK1 w kq - 1 7",
    // French Defense - blocked center, prophylaxis
    "rnbqk2r/ppp2pbp/3p1np1/8/3PP3/2N2NP1/PPP3PP/R1BQK2R w KQkq - 1 6",
    "rnb1k2r/ppp2pbp/3p1np1/8/3PP3/2N2NP1/PPP3PP/R1BQK2R w KQkq - 2 7",
    "rnb1k2r/ppp2pbp/3p1np1/8/3PPP2/2N3P1/PPP4P/R1BQK2R b KQkq - 0 7",
    // Caro-Kann - solid pawn structure
    "rnbqk2r/ppp1ppbp/5np1/3p4/3PP3/2N2NP1/PPP3PP/R1BQK2R w KQkq - 1 6",
    "rnb1k2r/ppp1ppbp/5np1/3p4/3PP3/2N2NP1/PPP3PP/R1BQK2R w KQkq - 2 7",
    "rnb1k2r/ppp1ppbp/5np1/3p4/3PPP2/2N3P1/PPP4P/R1BQK2R b KQkq - 0 7",
    // Slav Defense - space and structure
    "rnbqk2r/pp2ppbp/2npp1p1/2p5/2PP4/2N2NP1/PP3P1P/R1BQKB1R w KQkq - 1 8",
    "rnb1k2r/pp2ppbp/2npp1p1/2p5/2PP4/2N2NP1/PP3P1P/R1BQKB1R w KQkq - 2 9",
    "rnb1k2r/pp3pbp/2npppn1/2p5/2PP4/2N2NP1/PP3P1P/R1BQKB1R w KQkq - 2 9",
    // Queen's Gambit Declined - positional pressure
    "rnbqk2r/ppp2pbp/3pp1p1/2P5/2P5/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 1 7",
    "rnb1k2r/ppp2pbp/3pp1p1/2P5/2P5/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 2 8",
    // Catalan - bishop pair and space
    "rnbqk2r/ppp1ppbp/5n2/3p4/2PP4/6P1/PP2PPBP/RNBQK1NR w KQkq - 2 4",
    "rnb1k2r/ppp1ppbp/5n2/3p4/2PP4/6P1/PP2PPBP/RNBQK1NR w KQkq - 3 5",
    // King's Indian - fianchettoed kingside structure
    "rnbqk2r/pppp1pbp/5n2/4p1p1/2PP4/2N2NP1/PP2PP1P/R1BQKB1R w KQkq - 0 6",
    "rnb1k2r/pppp1pbp/5n2/4p1p1/2PP4/2N2NP1/PP2PP1P/R1BQKB1R w KQkq - 1 7",
    // Nimzo-Indian - positional compensation
    "r1bqk2r/pppp1pbp/2n2n2/4p1p1/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 1 7",
    "r1b1k2r/pppp1pbp/2n2n2/4p1p1/2PP4/2N2N2/PP2PPPP/R1BQKB1R w KQkq - 2 8",
    // English Opening - maneuvering positions
    "rnbqkb1r/ppp1pppp/5n2/3p4/2P5/2N2N2/PP1PPPPP/R1BQKB1R w KQkq - 2 4",
    "rnb1kb1r/ppp1pppp/5n2/3p4/2P5/2N2N2/PP1PPPPP/R1BQKB1R w KQkq - 3 5",
    // Réti Opening - hypermodern structure
    "rnbqkb1r/ppp1pppp/5n2/3p4/2P5/2N2NP1/PP1PPP1P/R1BQKB1R b KQkq - 1 4",
    "rnb1kb1r/ppp1pppp/5n2/3p4/2P5/2N2NP1/PP1PPP1P/R1BQKB1R w KQkq - 2 5",
    // Grünfeld - central tension
    "rnbqk2r/ppp1ppbp/3p1np1/8/2PP4/2N2NP1/PP2PP1P/R1BQKB1R w KQkq - 1 6",
    "rnb1k2r/ppp1ppbp/3p1np1/8/2PP4/2N2NP1/PP2PP1P/R1BQKB1R w KQkq - 2 7",
    // Benko Gambit - long-term play
    "rnbqkb1r/p1pppppp/5n2/1p6/2PP4/2N2NP1/PP2PP1P/R1BQKB1R w KQkq - 1 5",
    "rnb1kb1r/p1pppppp/5n2/1p6/2PP4/2N2NP1/PP2PP1P/R1BQKB1R w KQkq - 2 6",
    // Sicilian structures - positional middlegames
    "r1bqk2r/pp2ppbp/2npp1p1/2p5/3PP3/2N2NP1/PPP3PP/R1BQK2R w KQkq - 0 7",
    "r1b1k2r/pp2ppbp/2npp1p1/2p5/3PP3/2N2NP1/PPP3PP/R1BQK2R w KQkq - 1 8",
    "r1b1k2r/pp3pbp/2nppnp1/2p5/3PP3/2N2NP1/PPP3PP/R1BQK2R w KQkq - 1 8",
    // Scandinavian - open but positional
    "rnbqk2r/ppp1ppbp/5n2/3p4/3PP3/2N2NP1/PPP3PP/R1BQK2R b KQkq - 1 5",
    "rnb1k2r/ppp1ppbp/5n2/3p4/3PP3/2N2NP1/PPP3PP/R1BQK2R w KQkq - 2 6",
    // Philidor - quiet positional squeeze
    "rnbqk2r/ppppp1bp/5n2/5p2/4P3/2N2N2/PPPP1PPP/R1BQK2R w KQkq - 1 5",
    "rnb1k2r/ppppp1bp/5n2/5p2/4P3/2N2N2/PPPP1PPP/R1BQK2R w KQkq - 2 6",
    // Pirc Defense - central control struggle
    "rnbqk2r/ppp1ppbp/3p1np1/8/3PP3/2N2NP1/PPP3PP/R1BQK2R w KQkq - 1 6",
    "rnb1k2r/ppp1ppbp/3p1np1/8/3PP3/2N2NP1/PPP3PP/R1BQK2R w KQkq - 2 7",
    // Modern Defense - flexible structure
    "rnbqkb1r/ppp1pppp/6n1/3p4/3PP3/2N2NP1/PPP3PP/R1BQKB1R w KQkq - 1 5",
    "rnb1kb1r/ppp1pppp/6n1/3p4/3PP3/2N2NP1/PPP3PP/R1BQKB1R w KQkq - 2 6",
    // Closed Sicilian - maneuvering game
    "rnbqkbnr/pp1ppppp/8/2p5/2P5/2N2NP1/PP1PPP1P/R1BQKB1R b KQkq - 2 3",
    "rnbqkb1r/pp1ppppp/5n2/2p5/2P5/2N2NP1/PP1PPP1P/R1BQKB1R w KQkq - 3 4",
    // Tarrasch Defense - weak center dynamic
    "rnbqkb1r/pp2pppp/3p1n2/8/3PP3/2N2NP1/PPP3PP/R1BQKB1R w KQkq - 1 5",
    "rnb1kb1r/pp2pppp/3p1n2/8/3PP3/2N2NP1/PPP3PP/R1BQKB1R w KQkq - 2 6",
    // Old Indian - solid and positional
    "rnbqkb1r/pppppppp/5n2/8/3P4/2N2NP1/PPP1PP1P/R1BQKB1R b KQkq - 2 3",
    "rnbqk2r/ppppppbp/5n2/8/3P4/2N2NP1/PPP1PP1P/R1BQKB1R w KQkq - 3 4",
    // Queen's Indian - flexible pawn play
    "rnbqkb1r/pppp1ppp/4pn2/8/2PP4/2N2NP1/PPP1PP1P/R1BQKB1R w KQkq - 1 5",
    "rnb1kb1r/pppp1ppp/4pn2/8/2PP4/2N2NP1/PPP1PP1P/R1BQKB1R w KQkq - 2 6",
    // Positions with opposite-colored bishops
    "6k1/ppp2ppp/3p1n2/8/3PP3/2N2NP1/PPP3PP/R1BQK3 w Q - 1 10",
    "6k1/ppp3pp/3pppn1/8/3PP3/2N2NP1/PPP3PP/R1BQK3 w Q - 1 11",
    // Positions with weak squares to exploit
    "r1b1k2r/pppp1ppp/2n2n2/4p3/2PPP3/2N2N2/PPP2PPP/R1BQK2R w KQkq - 1 7",
    "r1b1k2r/pppp1ppp/2n4n/4p3/2PPP3/2N2NP1/PPP3PP/R1BQK2R w KQkq - 2 8",
];
