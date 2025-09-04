//#![allow(dead_code)]

mod attack_masks;
mod engine;
mod game;
mod game_state;
mod mechanics;
mod constants;
use game::*;
use macroquad::prelude::*;
use game_state::*;

#[macroquad::main(window_conf)]
async fn main() {
    macroquad::window::request_new_screen_size(WINDOW_WIDTH, WINDOW_HEIGHT);

    let textures = PieceTextures::new().await;
    let aptos: Font = load_ttf_font("assets/fonts/aptos-light.ttf")
        .await
        .expect("failed to load font");

    let mut game_state = GameState::initialise();
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
        next_frame().await
    }
}
