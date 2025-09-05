#![allow(dead_code)]
#![allow(unused_imports)]
mod attack_masks;
mod constants;
mod engine;
mod game;
mod game_state;
mod mechanics;
mod tests;

use game::*;
use macroquad::prelude::*;
use game_state::*;
use std::time::Instant;
use crate::tests::*;

use macroquad::time::*;
use ::rand::{rng, Rng};
use ::rand::seq::SliceRandom;
use std::time::Duration;
use macroquad::time::*;


// difference: 1. d3 e6 2. Kd2 Bb4

// matched to depth 4, a few different at depth 5 (e2e3 start)
const START_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBKQBNR w KQkq - 0 1"; 

const NO_PAWNS: &str = "rnbqkbnr/8/8/8/8/8/8/RNBKQBNR w KQkq - 0 1";

// matched to depth 3. different at 4, crashes at 5
const CASTLING: &str = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1";

// matched to depth 3
const EN_PASSANT: &str = "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPP2PPP/RNBQKBNR w KQkq d6 0 3";

// bb misfire (instant)
const PROMOTION: &str = "8/P7/8/8/8/8/7p/7k w - - 0 1";

// division by zero
const PINS: &str = "4k3/8/8/8/3n4/8/3R4/4K3 w - - 0 1";

// matched to depth 2, slightly different at depth 3
const SINGLE_CHECK: &str = "rnb1kbnr/pppp1ppp/8/4q3/8/4P3/PPPP1PPP/RNBQKBNR w KQkq - 2 3";
const SINGLE_CHECK_A2A3: &str = "rnb1kbnr/pppp1ppp/8/4q3/8/P3P3/1PPP1PPP/RNBQKBNR b KQkq - 2 3";
const SINGLE_CHECK_A2A3_E5B2: &str = "rnb1kbnr/pppp1ppp/8/8/8/P3P3/1qPP1PPP/RNBQKBNR w KQkq - 2 3";

// difference at depth 2. bb misfire after f8b4
const DOUBLE_CHECK: &str = "rnbqkb1r/pppp1ppp/5n2/4N3/4P3/8/PPP2PPP/RNBQKB1R b KQkq - 0 4";

// misfire after e2e8
const DISCOVERED_CHECK: &str = "4k3/8/8/8/8/3B4/4R3/4K3 w - - 0 1";




#[macroquad::main(window_conf)]
async fn main() {

    macroquad::window::request_new_screen_size(WINDOW_WIDTH, WINDOW_HEIGHT);

    let textures = PieceTextures::new().await;
    let aptos: Font = load_ttf_font("assets/fonts/aptos-light.ttf")
        .await
        .expect("failed to load font");

    let mut game_state = GameState::from_fen(SINGLE_CHECK);
    let mut board = Board::initialise();

    loop {
        if kill_game() {
            break;
        }
        clear_background(BLACK);
        
        board.draw_to_screen(&game_state, &textures, &aptos);
        board.update(&mut game_state, mouse_position().into());
        game_state.bitboards.draw_attack_masks_to_screen(&aptos);
        draw_framerate(&aptos);

        next_frame().await;
    }
}
    /* //normal play
    loop {
        if kill_game() {
            break;
        }
        clear_background(BLACK);

        board.draw_to_screen(&game_state, &textures, &aptos);
        board.update(&mut game_state, mouse_position().into());
        game_state.bitboards.draw_attack_masks_to_screen(&aptos);
        draw_framerate(&aptos);
        next_frame().await
    }
     
}

*/

/* 
fn main() {
    call_perft(NO_PAWNS, 6);
}
// */


fn call_perft(fen: &str, depth: usize) {

    let (total_positions, stockfish_positions, total_time_ms, sf_time_ms) = perft_divide(fen, depth);

    let nps = total_positions / total_time_ms;
    let sf_nps = stockfish_positions / sf_time_ms;
    println!("\n\n-------------------------------RESULTS-------------------------------\n\n");
    println!("Total positions up to depth {depth} is {total_positions}. Stockfish: {stockfish_positions}");
    println!("Time taken: {total_time_ms}ms. Stockfish: {sf_time_ms}ms.");
    println!("Nodes per millisecond: {nps}. Stockfish: {sf_nps}.");
}
