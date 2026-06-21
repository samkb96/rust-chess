use crate::constants::misc::fen_positions::AGGREGATE_TESTS;
use crate::game_state::GameState;

#[test]
pub fn compare_individual_masks() {
    let piece_names = ["Pawn", "Knight", "Bishop", "Rook", "Queen", "King"];
    let colour_names = ["White", "Black"];

    fn recursive_check(
        original: &mut GameState,
        incremental: &mut GameState,
        depth: usize,
        max_depth: usize,
        piece_names: &[&str],
        colour_names: &[&str],
    ) {
        if depth >= max_depth {
            return;
        }

        let moves = original.legal_moves();
        for move_to_make in moves {
            original.make_move(move_to_make);
            incremental.make_move(move_to_make);

            // Check each piece kind for each colour
            for colour in 0..2 {
                for piece_kind in 0..6 {
                    let orig = original.bitboards.attacked_by[colour][piece_kind];
                    let incr = incremental.bitboards.attacked_by[colour][piece_kind];

                    if orig != incr {
                        eprintln!(
                            "\n{} {} mismatch after move {} at depth {}\n\nBoard position: \n{}\n\nIncremental position: \n{}\n\nExpected attack masks:\n{}\nGot:\n{}",
                            colour_names[colour],
                            piece_names[piece_kind],
                            move_to_make,
                            depth,
                            format_pieces(&original.bitboards.pieces),
                            format_pieces(&incremental.bitboards.pieces),
                            format_bitboard(orig),
                            format_bitboard(incr)
                        );
                        panic!(
                            "{} {} mismatch at depth {}",
                            colour_names[colour], piece_names[piece_kind], depth
                        );
                    }
                }
            }

            // Recurse
            recursive_check(
                original,
                incremental,
                depth + 1,
                max_depth,
                piece_names,
                colour_names,
            );

            original.unmake_move();
            incremental.unmake_move();
        }
    }

    for fen in AGGREGATE_TESTS {
        let mut original_state = GameState::from_fen(fen);
        let mut incremental_state = GameState::from_fen(fen);

        recursive_check(
            &mut original_state,
            &mut incremental_state,
            0,
            4,
            &piece_names,
            &colour_names,
        );
    }
}
fn format_bitboard(bb: u64) -> String {
    let mut output = String::from("  a b c d e f g h\n");
    for rank in (0..8).rev() {
        output.push_str(&format!("{} ", rank + 1));
        for file in 0..8 {
            let square = rank * 8 + file;
            if (bb & (1u64 << square)) != 0 {
                output.push('X');
            } else {
                output.push('.');
            }
            output.push(' ');
        }
        output.push('\n');
    }
    output
}

fn format_pieces(pieces: &[[u64; 6]; 2]) -> String {
    let piece_chars = ['P', 'N', 'B', 'R', 'Q', 'K'];
    let mut board = vec![vec!['.'; 8]; 8];

    for piece_kind in 0..6 {
        let bb = pieces[0][piece_kind];
        for square in 0..64 {
            if (bb & (1u64 << square)) != 0 {
                let rank = square / 8;
                let file = square % 8;
                board[7 - rank][file] = piece_chars[piece_kind];
            }
        }
    }

    for piece_kind in 0..6 {
        let bb = pieces[1][piece_kind];
        for square in 0..64 {
            if (bb & (1u64 << square)) != 0 {
                let rank = square / 8;
                let file = square % 8;
                board[7 - rank][file] = (piece_chars[piece_kind] as u8 + 32) as char;
            }
        }
    }

    let mut output = String::from("  a b c d e f g h\n");
    for (rank_idx, row) in board.iter().enumerate() {
        let rank_num = 8 - rank_idx;
        output.push_str(&format!("{} ", rank_num));
        for piece in row {
            output.push(*piece);
            output.push(' ');
        }
        output.push('\n');
    }
    output
}
