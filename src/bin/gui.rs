// macroquad gui mode for ordinary games played on board between a combination of humans and bots
use bradybot::constants::magic::generate_static_magics;
use bradybot::constants::misc::fen_positions::*;
use bradybot::constants::start_fens::TEST_POSITIONS_SUITE;
use bradybot::engine::bot_handler::{Bot, SearchThreadHandler};
use bradybot::game_state::*;
use bradybot::mechanics::PieceColour;
use bradybot::modes::gui::*;
use bradybot::modes::mode_selection::{GameMode, parse_args};

use macroquad::prelude::*;
use std::env;
use std::sync::Arc;

#[macroquad::main(window_conf)]
async fn main() {
    let args: &[String] = &env::args().collect::<Vec<String>>();
    let game_mode = parse_args(args).unwrap_or_else(|err| panic!("{err}"));
    generate_static_magics();

    match game_mode {
        GameMode::BotVsBot {
            white,
            black,
            clock,
        } => {
            run_gui_bot_vs_bot(&white, &black, clock).await;
        }
        GameMode::HumanVsBot {
            bot,
            bot_colour,
            clock,
        } => {
            run_gui_human_vs_bot(START_FEN, &bot, bot_colour, clock).await;
        }
        GameMode::HumanVsHuman { fen, clock } => {
            run_gui_human_vs_human(&fen, clock).await;
        }
        _ => {
            panic!("incorrect binary setup")
        }
    }
}

async fn run_gui_human_vs_human(fen: &str, mut clock: Clock) {
    request_new_screen_size(WINDOW_WIDTH, WINDOW_HEIGHT);
    let textures = PieceTextures::new().await;
    let aptos: Font = load_ttf_font("assets/fonts/aptos-light.ttf").await.unwrap();
    let veramono: Font = load_ttf_font("assets/fonts/VeraMono.ttf").await.unwrap();

    let mut game_state = GameState::from_fen(fen);
    let mut board = Board::initialise();

    loop {
        clock.tick(game_state.side_to_move);

        if is_key_pressed(KeyCode::Z) {
            game_state.unmake_move();
        }

        if kill_game() {
            break;
        }

        clear_background(BLACK);

        board.draw_human_game_to_screen(&game_state, &textures, &aptos, &clock, &veramono);
        board.update_from_human(
            &mut game_state,
            mouse_position().into(),
            &textures,
            &mut clock,
        );
        next_frame().await;
    }
}

async fn run_gui_bot_vs_bot(white_bot: &Arc<Bot>, black_bot: &Arc<Bot>, mut clock: Clock) {
    request_new_screen_size(WINDOW_WIDTH, WINDOW_HEIGHT);
    let textures = PieceTextures::new().await;
    let aptos: Font = load_ttf_font("assets/fonts/aptos-light.ttf").await.unwrap();
    let veramono: Font = load_ttf_font("assets/fonts/VeraMono.ttf").await.unwrap();

    let random_fen = TEST_POSITIONS_SUITE[fastrand::usize(..500)];

    let mut game_state = GameState::from_fen(random_fen);
    let mut board = Board::initialise();
    let mut search_thread_handler = SearchThreadHandler::default();

    loop {
        clock.tick(game_state.side_to_move);

        if kill_game() {
            break;
        }

        clear_background(BLACK);

        board.draw_bot_game_to_screen(&game_state, &textures, &aptos, &mut clock, &veramono);
        match game_state.side_to_move {
            PieceColour::White => board.update_from_bot(
                &mut game_state,
                white_bot,
                &mut search_thread_handler,
                &mut clock,
            ),
            PieceColour::Black => board.update_from_bot(
                &mut game_state,
                black_bot,
                &mut search_thread_handler,
                &mut clock,
            ),
        }
        next_frame().await;
    }
}

async fn run_gui_human_vs_bot(
    fen: &str,
    bot: &Arc<Bot>,
    bot_colour: PieceColour,
    mut clock: Clock,
) {
    request_new_screen_size(WINDOW_WIDTH, WINDOW_HEIGHT);
    let textures = PieceTextures::new().await;
    let aptos: Font = load_ttf_font("assets/fonts/aptos-light.ttf").await.unwrap();
    let veramono: Font = load_ttf_font("assets/fonts/VeraMono.ttf").await.unwrap();

    let mut game_state = GameState::from_fen(fen);
    let mut board = Board::initialise();
    let mut search_thread_handler = SearchThreadHandler::default();

    loop {
        clock.tick(game_state.side_to_move);

        if kill_game() {
            break;
        }

        clear_background(BLACK);

        board.draw_human_game_to_screen(&game_state, &textures, &aptos, &clock, &veramono);

        if game_state.side_to_move == bot_colour {
            board.update_from_bot(&mut game_state, bot, &mut search_thread_handler, &mut clock)
        }

        if game_state.side_to_move != bot_colour {
            board.update_from_human(
                &mut game_state,
                mouse_position().into(),
                &textures,
                &mut clock,
            );
        }

        next_frame().await;
    }
}
