use crate::engine_handler::Bot;
use std::fmt;
pub enum GameMode {
    #[cfg(feature = "gui")]
    HumanVsBot, // TODO configurable bot opponent
    #[cfg(feature = "gui")]
    BotVsBot {
        white: Bot,
        black: Bot,
    },
    BotMatch {
        white: Bot,
        black: Bot,
    },
    Perft {
        fen: String,
        depth: usize,
    },
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
                write!(f, "Invalid bot selection: '{}'. Use: random, negamax, alphabeta", bot)   
            },
            GME::InvalidGameMode(mode) => {
                write!(f, "Invalid mode selection: '{}'. Use: human, bvb, match, perft", mode)
            },
            GME::MissingArgument(arg) => {
                write!(f, "Missing required argument: '{}'", arg)   
            },
            GME::InvalidArgumentValue{ arg, expected} => {
                write!(f, "Invalid argument for '{}': '{}'", expected, arg)   
            }
        }
    }
}

pub fn parse_args(args: &[String]) -> Result<GameMode, GameModeError> {
    use GameModeError as GME;
    if args.len() < 2 { return Err(GME::MissingArgument("game mode".to_string())) }
    use GameMode as GM;
    match args[1].as_str() {
        "human" => 
            Ok(GM::HumanVsBot),
        "bvb" => {
            let (white, black) = select_bots_from_args(args)?;
            Ok(GM::BotVsBot { white, black })
        }
        "match" => {
            let (white, black) = select_bots_from_args(args)?;
            Ok(GM::BotMatch { white, black })
        }
        "perft" => {
            select_perft_from_args(args)
        }
        _ => 
            Err(GME::InvalidGameMode(args[1].to_string()))
    }
}

fn select_perft_from_args(args: &[String]) -> Result<GameMode, GameModeError> {
    use GameModeError as GME;
    if args.len() < 3 { 
        return Err(GME::MissingArgument("perft fen".to_string()))
    } 
    if args.len() == 3 {
        return Err(GME::MissingArgument("search depth".to_string()))
    }

    let fen = args[3].clone();
    if let Ok(depth) = args[4].parse::<usize>() { 
        return Ok(GameMode::Perft{ fen, depth }) 
    } else {
        return Err(GME::InvalidArgumentValue { arg: (args[4].to_string()), expected: "usize".to_string() }) 
    }
}

fn select_bots_from_args(args: &[String]) -> Result<(Bot, Bot), GameModeError> {
    use GameModeError as GME;
    if args.len() < 4 {
        return Err(GME::InvalidBotSelection("".to_string()));
    }

    let white = Bot::create(args[2].as_str())?;
    let black = Bot::create(args[3].as_str())?;

    Ok((white, black))
}
