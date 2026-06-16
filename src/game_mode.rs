use crate::engine_handler::Bot;
use crate::mechanics::PieceColour;
use std::fmt;

pub enum GameMode {
    HumanVsBot { bot: Bot, bot_colour: PieceColour }, // gui
    BotVsBot { white: Bot, black: Bot },              // gui
    BotArena { white: Bot, black: Bot },              // cli
    Perft { fen: String, depth: usize },              // cli
}

#[derive(Debug)]
pub enum GameModeError {
    InvalidBotSelection(String),
    InvalidGameMode(String),
    MissingArgument(String),
    InvalidArgumentValue { arg: String, expected: String },
}

impl fmt::Display for GameModeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        use GameModeError as GME;
        match self {
            GME::InvalidBotSelection(bot) => {
                write!(
                    f,
                    "Invalid bot selection: '{bot}'. Use: random, negamax, alphabeta, pst"
                )
            }
            GME::InvalidGameMode(mode) => {
                write!(
                    f,
                    "Invalid mode selection: '{mode}'. Use: human, bvb, match, perft"
                )
            }
            GME::MissingArgument(arg) => {
                write!(f, "Missing required argument: '{arg}'")
            }
            GME::InvalidArgumentValue { arg, expected } => {
                write!(f, "Invalid argument for '{expected}': '{arg}'")
            }
        }
    }
}

pub fn parse_args(args: &[String]) -> Result<GameMode, GameModeError> {
    use GameMode as M;
    use GameModeError as E;

    if args.len() < 2 {
        return Err(E::MissingArgument("game mode".to_string()));
    }

    match args[1].as_str() {
        "human" => select_bot_and_colour_from_args(args),
        "bvb" => {
            let (white, black) = select_bots_from_args(args)?;
            Ok(M::BotVsBot { white, black })
        }
        "arena" => {
            let (white, black) = select_bots_from_args(args)?;
            Ok(M::BotArena { white, black })
        }
        "perft" => select_perft_from_args(args),
        _ => Err(E::InvalidGameMode(args[1].to_string())),
    }
}

fn select_bot_and_colour_from_args(args: &[String]) -> Result<GameMode, GameModeError> {
    use GameModeError as E;
    if args.len() < 3 {
        return Err(E::InvalidBotSelection("".to_string()));
    }

    // let the bot play black if colour argument provided
    let bot = Bot::create(args[2].as_str())?; // propagate invalid bot selection error

    let mut bot_colour = PieceColour::Black;

    if args.len() >= 4 {
        match args[3].as_ref() {
            "white" => {
                bot_colour = PieceColour::White;
            }
            "black" => (),
            _ => {
                return Err(E::InvalidArgumentValue {
                    arg: args[3].clone(),
                    expected: "bot colour".to_string(),
                });
            }
        }
    }

    Ok(GameMode::HumanVsBot { bot, bot_colour })
}

fn select_perft_from_args(args: &[String]) -> Result<GameMode, GameModeError> {
    use GameModeError as GME;

    if args.len() < 8 {
        return Err(GME::MissingArgument("perft fen".to_string()));
    }
    if args.len() == 8 {
        return Err(GME::MissingArgument("search depth".to_string()));
    }

    let fen = args[2..8].join(" ");

    println!("{}", &fen);

    if let Ok(depth) = args[8].parse::<usize>() {
        return Ok(GameMode::Perft { fen, depth });
    }

    Err(GME::InvalidArgumentValue {
        arg: (args[4].to_string()),
        expected: "usize".to_string(),
    })
}

fn select_bots_from_args(args: &[String]) -> Result<(Bot, Bot), GameModeError> {
    use GameModeError as GME;
    if args.len() < 4 {
        return Err(GME::InvalidBotSelection("".to_string()));
    }

    let white = Bot::create(args[2].as_str())?; // propagate invalid bot selection error
    let black = Bot::create(args[3].as_str())?;

    Ok((white, black))
}
