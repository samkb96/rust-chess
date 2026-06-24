use crate::engine::bot_handler::Bot;
use crate::game_state::{Clock, Seconds};
use crate::mechanics::PieceColour;
use std::fmt;
use std::sync::Arc;

pub enum GameMode {
    HumanVsHuman {
        fen: String,
        clock: Clock,
    },
    HumanVsBot {
        bot: Arc<Bot>,
        bot_colour: PieceColour,
        clock: Clock,
    }, // gui
    BotVsBot {
        white: Arc<Bot>,
        black: Arc<Bot>,
        clock: Clock,
    }, // gui
    BotArena {
        white: Arc<Bot>,
        black: Arc<Bot>,
    }, // cli
    Perft {
        fen: String,
        depth: usize,
    }, // cli
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
    use GameModeError as E;

    if args.len() < 2 {
        return Err(E::MissingArgument("game mode".to_string()));
    }

    match args[1].as_str() {
        "hvh" => parse_args_human_vs_human(args),
        "hvb" => parse_args_human_vs_bot(args),
        "bvb" => parse_args_bot(args, false),
        "arena" => parse_args_bot(args, true),
        "perft" => select_perft_from_args(args),
        _ => Err(E::InvalidGameMode(args[1].to_string())),
    }
}

fn parse_args_human_vs_human(args: &[String]) -> Result<GameMode, GameModeError> {
    use GameModeError as E;
    if args.len() < 3 {
        return Err(E::InvalidArgumentValue {
            arg: "".to_string(),
            expected: "fen".to_string(),
        });
    }

    let fen = args[2..8].join(" ");
    let clock = Clock::new(300.0, 5.0);

    Ok(GameMode::HumanVsHuman { fen, clock })
}

fn parse_args_human_vs_bot(args: &[String]) -> Result<GameMode, GameModeError> {
    use GameModeError as E;
    if args.len() < 3 {
        return Err(E::InvalidBotSelection("".to_string()));
    }

    // let the bot play black if colour argument provided
    let bot = Bot::create(args[2].as_str())?; // propagate invalid bot selection error

    let mut bot_colour = PieceColour::Black;

    if args.len() >= 4 {
        match args[3].as_ref() {
            "w" => {
                bot_colour = PieceColour::White;
            }
            "b" => (),
            _ => {
                return Err(E::InvalidArgumentValue {
                    arg: args[3].clone(),
                    expected: "bot colour".to_string(),
                });
            }
        };
    }

    let clock = parse_clock_from_args(args, 4, 5)?;

    Ok(GameMode::HumanVsBot {
        bot,
        bot_colour,
        clock,
    })
}

fn parse_args_bot(args: &[String], arena: bool) -> Result<GameMode, GameModeError> {
    use GameModeError as E;

    if args.len() < 4 {
        return Err(E::InvalidBotSelection("".to_string()));
    }

    let white = Bot::create(args[2].as_str())?; // propagate invalid bot selection error
    let black = Bot::create(args[3].as_str())?;

    if arena {
        Ok(GameMode::BotArena { white, black })
    } else {
        let clock = parse_clock_from_args(args, 4, 5)?;
        Ok(GameMode::BotVsBot {
            white,
            black,
            clock,
        })
    }
}

fn select_perft_from_args(args: &[String]) -> Result<GameMode, GameModeError> {
    use GameModeError as E;

    if args.len() < 8 {
        return Err(E::MissingArgument("perft fen".to_string()));
    }
    if args.len() == 8 {
        return Err(E::MissingArgument("search depth".to_string()));
    }

    let fen = args[2..8].join(" ");

    println!("{}", &fen);

    if let Ok(depth) = args[8].parse::<usize>() {
        return Ok(GameMode::Perft { fen, depth });
    }

    Err(E::InvalidArgumentValue {
        arg: (args[4].to_string()),
        expected: "usize".to_string(),
    })
}

fn parse_clock_from_args(
    args: &[String],
    minutes_arg_index: usize,
    increment_arg_index: usize,
) -> Result<Clock, GameModeError> {
    use GameModeError as E;

    let Some(maybe_minutes) = args.get(minutes_arg_index) else {
        return Ok(Clock::new(60.0, 1.0)); // 1+1 unless otherwise specified
    };

    let Ok(minutes) = maybe_minutes.parse::<Seconds>() else {
        return Err(E::InvalidArgumentValue {
            arg: maybe_minutes.to_string(),
            expected: "start in minutes".to_string(),
        });
    };

    let Some(maybe_increment) = args.get(increment_arg_index) else {
        let default_increment = (minutes / 60.0).floor();
        return Ok(Clock::new(minutes, default_increment)); // assume 5+5, 3+3, 1+1 etc
    };

    let Ok(increment) = maybe_increment.parse::<Seconds>() else {
        return Err(E::InvalidArgumentValue {
            arg: maybe_increment.to_owned(),
            expected: "increment in seconds".to_string(),
        });
    };

    Ok(Clock::new(minutes * 60.0, increment))
}
