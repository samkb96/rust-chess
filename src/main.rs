#![allow(dead_code)]

mod game;
mod mechanics;
use game::*;
use macroquad::prelude::*;
use mechanics::*;

#[macroquad::main("chess")]
async fn main() {
    
    // size window
    macroquad::window::request_new_screen_size(WINDOW_WIDTH, WINDOW_HEIGHT);
    // preload textures, font
    let textures = PieceTextures::new().await;
    let aptos: Font = load_ttf_font("assets/fonts/aptos-light.ttf").await.unwrap();
    // initialise bitboards
    let bitboards = BitBoards::from_fen(STARTING_POSITION_FEN);
    let mut board = DisplayBoard::from_bitboards(&bitboards);

    loop {
        if kill_game() {
            break
        }
        clear_background(BLACK);
        board.update();
        // pass bitboards to render and draw
        board.draw_to_screen(&textures, &aptos);
        board.draw_debug_info(&aptos);
        draw_framerate(&aptos);
        bitboards.print_bitboards_to_screen(&aptos);

        next_frame().await
    }
}
