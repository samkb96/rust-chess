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

use crate::engine_handler::Bot;
use crate::tests::*;
use game::*;
use game_state::*;
use macroquad::prelude::*;
use std::env;

const START_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
const MIDGAME_TEST: &str = "r1bqk2r/pp1p1ppp/2n1pn2/8/1bPPP3/2N2N2/PPQ2PPP/R1B1KB1R w KQkq - 1 8";

// gui mode for ordinary games played on board between a combination of humans and bots
#[cfg(feature = "gui")]
#[macroquad::main(window_conf)]
async fn main() {
    let args: Vec<String> = env::args().collect();

    // parse commands to determine which bots are to play
    let game_mode = &args[1];
    if game_mode == "bvb" {
        println!("bot game selected");
        let white_bot_choice = args[2].as_str();
        let black_bot_choice = args[3].as_str();

        let white_bot = Bot::create(white_bot_choice);
        let black_bot = Bot::create(black_bot_choice);

        run_gui(START_FEN, game_mode != "bvb", &white_bot, &black_bot).await;
    }
}

// non-gui mode for performance testing and bot vs bot matches
#[cfg(not(feature = "gui"))]
fn main() {
    call_perft(MIDGAME_TEST, 4)
}

async fn run_gui(fen: &str, human_input: bool, white_bot: &Bot, black_bot: &Bot) {
    request_new_screen_size(WINDOW_WIDTH, WINDOW_HEIGHT);
    let textures = PieceTextures::new().await;
    let aptos: Font = load_ttf_font("assets/fonts/aptos-light.ttf")
        .await
        .expect("failed to load font");

    let mut game_state = GameState::from_fen(fen);
    let mut board = Board::initialise();

    if human_input {
        loop {
            if kill_game() {
                break;
            }

            clear_background(BLACK);

            board.draw_human_game_to_screen(&game_state, &textures, &aptos);
            board.update_human_game(&mut game_state, mouse_position().into());
            draw_framerate(&aptos);

            next_frame().await;
        }
    } else {
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
}

fn call_perft(fen: &str, depth: usize) {
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
