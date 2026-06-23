use bradybot::constants::magic::generate_static_magics;
use bradybot::modes::bot_arena::bot_arena;
use bradybot::modes::mode_selection::GameMode;
use bradybot::modes::mode_selection::parse_args;
use bradybot::modes::perft::call_perft;
use std::env;

// non-gui mode for performance testing and bot vs bot matches
fn main() {
    let args: &[String] = &env::args().collect::<Vec<String>>();
    let game_mode = parse_args(args).unwrap_or_else(|err| panic!("{err}"));
    generate_static_magics();

    match game_mode {
        GameMode::BotArena { white, black } => bot_arena(&white, &black),
        GameMode::Perft { fen, depth } => call_perft(&fen, depth),
        _ => {
            panic!("You've set up the binaries logic incorrectly")
        }
    }
}
