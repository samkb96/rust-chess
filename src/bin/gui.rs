// macroquad gui mode for ordinary games played on board between a combination of humans and bots
#[allow(unused_imports)]
use bradybot::constants::fen_positions::*;

use bradybot::bot_handler::Bot;
use bradybot::game::*;
use bradybot::game_mode::GameMode;
use bradybot::game_mode::parse_args;
use bradybot::game_state::*;
use bradybot::mechanics::PieceColour;

use macroquad::prelude::*;
use std::env;

#[macroquad::main(window_conf)]
async fn main() {
    let args: &[String] = &env::args().collect::<Vec<String>>();
    let game_mode = parse_args(args).unwrap_or_else(|err| panic!("{err}"));

    match game_mode {
        GameMode::BotVsBot { white, black } => {
            run_gui_bot_vs_bot(START_FEN, &white, &black).await;
        }
        GameMode::HumanVsBot { bot, bot_colour } => {
            run_gui_human_vs_bot(START_FEN, &bot, bot_colour).await;
        }
        _ => {
            panic!("incorrect binary setup")
        }
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

async fn run_gui_human_vs_bot(fen: &str, bot: &Bot, bot_colour: PieceColour) {
    request_new_screen_size(WINDOW_WIDTH, WINDOW_HEIGHT);
    let textures = PieceTextures::new().await;
    let aptos: Font = load_ttf_font("assets/fonts/aptos-light.ttf")
        .await
        .expect("failed to load font");

    let mut game_state = GameState::from_fen(fen);
    let mut board = Board::initialise();

    loop {
        if kill_game() {
            break;
        }

        clear_background(BLACK);

        board.draw_human_game_to_screen(&game_state, &textures, &aptos);
        board.update_human_game(
            &mut game_state,
            mouse_position().into(),
            bot,
            bot_colour,
            &textures,
        );

        next_frame().await;
    }
}
