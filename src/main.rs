#![allow(dead_code)]

mod game;
mod mechanics;
mod attack_masks;
use game::*;
use macroquad::prelude::*;
use mechanics::*;

#[macroquad::main(window_conf)]
async fn main() {
    macroquad::window::request_new_screen_size(WINDOW_WIDTH, WINDOW_HEIGHT);

    let textures = PieceTextures::new().await;
    let aptos: Font = load_ttf_font("assets/fonts/aptos-light.ttf").await.unwrap();

    let game_state = GameState::initialise();
    let mut board = DisplayBoard::initialise(game_state);

    loop {
        if kill_game() {
            break;
        };

        clear_background(BLACK);

        board.update();

        board.draw_to_screen(&textures, &aptos);
        board.draw_debug_info(&aptos);
        draw_framerate(&aptos);
        board.game_state.print_to_screen(&aptos);

        next_frame().await
    }
}
