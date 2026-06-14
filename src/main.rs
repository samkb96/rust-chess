#![allow(dead_code)]
#![allow(unused_imports)]
mod attack_masks;
mod constants;
mod engines;
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
use std::env;

const START_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
const MIDGAME_TEST: &str = "r1bqk2r/pp1p1ppp/2n1pn2/8/1bPPP3/2N2N2/PPQ2PPP/R1B1KB1R w KQkq - 1 8";

#[cfg(not(feature = "perft"))]
#[macroquad::main(window_conf)] 
async fn main() {
    let args: Vec<String> = env::args().collect();
    dbg!(&args);
    // parse commands to determine which bots are to play
    let game_mode = &args[1];
    if game_mode == "bvb" {
        let white_bot = &args[2];
        let black_bot = &args[3];
    }
    run_gui(START_FEN, game_mode == "bvb").await;
}

// perft mode for debugging / performance testing; accessed with cargo perft
#[cfg(feature = "perft")]
fn main() {
    call_perft(MIDGAME_TEST, 4)
}

// START POSITION CONSTANTS

async fn run_gui(fen: &str, human_input: bool) {
    request_new_screen_size(WINDOW_WIDTH, WINDOW_HEIGHT);
    let textures = PieceTextures::new().await;
    let aptos: Font = load_ttf_font("assets/fonts/aptos-light.ttf")
        .await
        .expect("failed to load font");

    let mut game_state = GameState::from_fen(fen);
    let mut board = Board::initialise(human_input);

    loop {
        if kill_game() { break; }

        clear_background(BLACK);

        board.draw_to_screen(&game_state, &textures, &aptos);
        board.update(&mut game_state, mouse_position().into());
        draw_framerate(&aptos);

        next_frame().await;
    }
}

fn call_perft(fen: &str, depth: usize) {

    let (
        total_positions,
        stockfish_positions,
        total_time_ms,
        sf_time_ms
    ) = perft_divide(fen, depth);

    let nps = if total_time_ms > 0 {total_positions / total_time_ms} else {0};
    let sf_nps = if sf_time_ms > 0 {stockfish_positions / sf_time_ms} else {0};
    println!("\n\n-------------------RESULTS-------------------\n\n");
    println!("Total positions up to depth {depth} is {total_positions}. Stockfish: {stockfish_positions}");
    println!("Time taken: {total_time_ms}ms. Stockfish: {sf_time_ms}ms.");
    println!("Nodes per millisecond: {nps}. Stockfish: {sf_nps}.");
}