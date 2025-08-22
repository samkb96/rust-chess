
mod game;
mod mechanics;
use game::*;
use macroquad::prelude::*;
use mechanics::*;

#[macroquad::main("chess")]
async fn main() {
    // size window
    macroquad::window::request_new_screen_size(WINDOW_SIZE, WINDOW_SIZE);
    // preload textures, font
    let textures = PieceTextures::new().await;
    let aptos: Font = load_ttf_font("misc/aptos-light.ttf").await.unwrap();
    // initialise bitboards
    let bitboards = BitBoards::from_fen(STARTING_POSITION_FEN);
    let mut board = DisplayBoard::from_bitboards(&bitboards);
    loop {
        clear_background(BLACK);
        // pass bitboards to render and draw
        board.draw_to_screen(&textures, &aptos);
        board.update();

        next_frame().await
    }
}
