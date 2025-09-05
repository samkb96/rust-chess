use arrayvec::ArrayString;

use crate::game_state::{self, *};
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};
use std::time::{Instant};

fn perft(game_state: &mut GameState, depth: usize) -> usize {
    if depth == 0 {
        return 1;
    }
    let mut nodes: usize = 0;
    let moves = game_state.legal_moves();

    for move_to_make in moves {
        
        game_state.make_move(move_to_make);

        nodes += perft(game_state, depth - 1);
        game_state.unmake_move();
    }
    nodes
}

pub fn perft_divide(fen: &str, depth: usize) -> (usize, usize, usize, usize) {
    let mut game_state = GameState::from_fen(fen);
    let first_moves = game_state.legal_moves();

    let mut my_counts: HashMap<String, usize> = HashMap::new();
    let mut my_total = 0;
    let start = Instant::now();

    for m in first_moves {
        let move_name_str = move_name(m).to_string();
        println!("Analysing {move_name_str}");
        game_state.make_move(m);
        let nodes = perft(&mut game_state, depth - 1);
        game_state.unmake_move();
        
        my_counts.insert(move_name_str.clone(), nodes);
        my_total += nodes;
    }
    let elapsed = start.elapsed();

    // stockfish
    let start = Instant::now();
    let sf_counts = stockfish_perft(fen, depth as u32).unwrap();
    let sf_elapsed = start.elapsed();
    let sf_total = sf_counts.values().sum::<usize>();
    let sf_nodes = sf_total / 2;

    // sort moves
    let mut moves: Vec<_> = my_counts.keys().collect();
    moves.sort();

    // print comparisons
    for mv in moves {
        let my_nodes = my_counts.get(mv).cloned().unwrap_or(0);

        //let sf_nodes = sf_counts.get(mv).cloned().unwrap_or(0);
        let indicator = if my_nodes == sf_counts[mv] { "" } else { " - different" };
        println!("{mv}: ({my_nodes}, {}){indicator}", sf_counts[mv]);
    }

    (my_total, sf_nodes, elapsed.as_millis() as usize, sf_elapsed.as_millis() as usize)
}

type StockfishPerft = std::io::Result<HashMap<String, usize>>;

pub fn stockfish_perft(fen: &str, depth: u32) -> StockfishPerft {

    let mut engine = Command::new("stockfish")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    let mut stdin = engine.stdin.take().unwrap();
    let stdout = engine.stdout.take().unwrap();
    let mut reader = BufReader::new(stdout);

    let send = |cmd: &str, stdin: &mut std::process::ChildStdin| {
        stdin.write_all(cmd.as_bytes()).unwrap();
        stdin.write_all(b"\n").unwrap();
        stdin.flush().unwrap();
    };
    
    // 1. Initialize engine
    send("uci", &mut stdin);

    send("isready", &mut stdin);

    // Wait for readyok
    let mut line = String::new();
    loop {
        line.clear();
        reader.read_line(&mut line)?;
        if line.trim() == "readyok" { break; }
    }

    // 2. Start new game & set position
    send("ucinewgame", &mut stdin);
    send(&format!("position fen {}", fen), &mut stdin);

    // 3. Run perft
    send(&format!("go perft {}", depth), &mut stdin);

    // 4. Read output
    let mut perft_map = HashMap::new();

    loop {

        line.clear();
        let bytes_read = reader.read_line(&mut line)?;
        if bytes_read == 0 { break; } // EOF
        let l = line.trim();

        // parse move counts (format: e2e4: 20)
        if let Some((mv, nodes)) = l.split_once(':') {
            if let Ok(nodes) = nodes.trim().parse::<usize>() {
                perft_map.insert(mv.to_string(), nodes);
            }
        }

        // stop reading once total nodes are printed
        if l.to_lowercase().starts_with("nodes") {
            break;
        }
    }

    // 5. Quit engine
    send("quit", &mut stdin);

    Ok(perft_map)
}

fn move_name(mv: Move) -> String {
    let mut name = String::new();
    let (start_file, start_rank) = (((mv.start_square % 8)  as u8 + b'a') as char, ((mv.start_square / 8) as u8 + b'1') as char);
    let (end_file, end_rank) = (((mv.end_square % 8) as u8 + b'a') as char, ((mv.end_square / 8) as u8 + b'1') as char);

    for character in [start_file, start_rank, end_file, end_rank] {
        name.push(character);
    }
    name
}

