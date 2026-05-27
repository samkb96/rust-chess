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

const START_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

#[cfg(not(feature = "perft"))]
#[macroquad::main(window_conf)]
async fn main() {
    run_gui(TEST_4).await;
}

// perft mode for debugging / performance testing; accessed with cargo perft
#[cfg(feature = "perft")]
fn main() {
    call_perft(TEST_5, 5)
}

// START POSITION CONSTANTS

async fn run_gui(fen: &str) {
    request_new_screen_size(WINDOW_WIDTH, WINDOW_HEIGHT);
    let textures = PieceTextures::new().await;
    let aptos: Font = load_ttf_font("assets/fonts/aptos-light.ttf")
        .await
        .expect("failed to load font");

    let mut game_state = GameState::from_fen(fen);
    let mut board = Board::initialise();

    loop {
        if kill_game() { break; }

        clear_background(BLACK);

        board.draw_to_screen(&game_state, &textures, &aptos);
        board.update(&mut game_state, mouse_position().into());
        game_state.bitboards.draw_attack_masks_to_screen(&aptos);
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

// TEST POSITIONS
// TODO: match all test positions to depth 6
// still need some for draws by 50 move / insufficient material / repetition

// castling, en passant, pins
// Match status: errors at depth 4 (eg a1b1)
const TEST_1: &str = "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";

// endgame, en passant edge cases
// Match status: errors at depth 2 (eg e2e4)
const TEST_2: &str = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";

// promotion
// Match status: errors at depth 2 (eg c4c5)
const TEST_3: &str = "r3k2r/Pppp1ppp/1b3nbN/nP6/2P1P3/8/Pp1P2PP/R2Q1RK1 w kq - 0 1";

// discovered check, promotion
// panics immediately
const TEST_4: &str = "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8";

// middlegame; more for actual performance testing
// matched to depth 5. depth 6 would probably take half an hour
const TEST_5: &str = "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10";
