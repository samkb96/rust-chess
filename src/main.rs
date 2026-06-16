

mod attack_masks;
mod constants;
mod engine_handler;
mod evaluators;
mod game;
mod game_mode;
mod game_state;
mod mechanics;
mod search_engines;
mod tests;

use engine_handler::Bot;
use game::*;
use game_mode::GameMode;
use game_state::*;
use macroquad::prelude::*;
use std::env;
use tests::*;

use crate::game_mode::parse_args;
use crate::mechanics::PieceColour;

const START_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
const MIDGAME_TEST: &str = "r1bqk2r/pp1p1ppp/2n1pn2/8/1bPPP3/2N2N2/PPQ2PPP/R1B1KB1R w KQkq - 1 8";

// gui mode for ordinary games played on board between a combination of humans and bots
#[cfg(feature = "gui")]
#[macroquad::main(window_conf)]
async fn main() {
    let args: &[String] = &env::args().collect::<Vec<String>>();
    let game_mode = parse_args(args).unwrap_or_else(|err| panic!("{err}"));

    match game_mode {
        GameMode::BotVsBot { white, black } => {
            run_gui_bot_vs_bot(START_FEN, &white, &black).await;
        }
        GameMode::HumanVsBot { bot, bot_colour } => {
            run_gui_human_vs_bot(START_FEN, &bot, &bot_colour).await;
        }
        _ => {
            panic!("You've set up the cfg(gui) incorrectly")
        }
    }
}

// non-gui mode for performance testing and bot vs bot matches
#[cfg(not(feature = "gui"))]
fn main() {
    let args: &[String] = &env::args().collect::<Vec<String>>();
    let game_mode = parse_args(args).unwrap_or_else(|err| panic!("{err}"));

    match game_mode {
        GameMode::BotMatch { white, black } => {
            todo!()
        }
        GameMode::Perft { fen, depth } => call_perft(fen, depth),
        _ => {
            panic!("You've set up the cfg(gui) incorrectly")
        }
    }
}

async fn run_gui_human_vs_bot(fen: &str, _bot: &Bot, _bot_colour: &PieceColour) {
    request_new_screen_size(WINDOW_WIDTH, WINDOW_HEIGHT);
    let textures = PieceTextures::new().await;
    let aptos: Font = load_ttf_font("assets/fonts/aptos-light.ttf")
        .await
        .expect("failed to load font");

    let mut game_state = GameState::from_fen(fen);
    let mut board = Board::initialise();

    loop {
        // TODO set this up with bot input
        // need a bot_to_move field in one of the types in game.rs, or something
        if kill_game() {
            break;
        }

        clear_background(BLACK);

        board.draw_human_game_to_screen(&game_state, &textures, &aptos);
        board.update_human_game(&mut game_state, mouse_position().into());
        draw_framerate(&aptos);

        next_frame().await;
    }
}

async fn run_gui_bot_vs_bot(fen: &str, white_bot: &Bot, black_bot: &Bot) {
    request_new_screen_size(WINDOW_WIDTH, WINDOW_HEIGHT);
    let textures = PieceTextures::new().await;
    let aptos: Font = load_ttf_font("assets/fonts/aptos-light.ttf")
        .await
        .expect("failed to load font");

    let mut game_state = GameState::from_fen(fen);
    let board = Board::initialise();

    loop {
        if kill_game() {
            break;
        }

        clear_background(BLACK);

        board.draw_bot_game_to_screen(&game_state, &textures, &aptos);
        board.update_bot_game(&mut game_state, white_bot, black_bot);

        next_frame().await;
    }
}

#[allow(dead_code)]
fn call_perft(fen: &str, depth: usize) {
    // TODO implement a fen subtype of &str at some point with impl fn validity_check
    // 1 king a side, no pawns on final ranks, etc

    let (total_positions, stockfish_positions, total_time_ms, sf_time_ms) =
        perft_divide(fen, depth);

    let nps = if total_time_ms > 0 {
        total_positions / total_time_ms
    } else {
        0
    };
    let sf_nps = if sf_time_ms > 0 {
        stockfish_positions / sf_time_ms
    } else {
        0
    };
    println!("\n\n-------------------RESULTS-------------------\n\n");
    println!(
        "Total positions up to depth {depth} is {total_positions}. Stockfish: {stockfish_positions}"
    );
    println!("Time taken: {total_time_ms}ms. Stockfish: {sf_time_ms}ms.");
    println!("Nodes per millisecond: {nps}. Stockfish: {sf_nps}.");
}
