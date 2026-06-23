use crate::mechanics::*;

// fen strings for intialisation of position
pub mod fen_positions {
    pub const START_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    pub const QGD_MIDGAME: &str =
        "rnbqkb1r/ppp2ppp/5n2/3pp3/2PP4/2N5/PPP2PPP/R1BQKBNR w KQkq - 1 4";
    pub const MATE_IN_FOUR: &str = "6k1/5ppp/8/4Q3/8/1B6/4K3/8 w - - 0 1";

    pub const AGGREGATE_TESTS: [&str; 10] = [
        "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1",
        "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3",
        "8/P5k1/8/8/8/8/6K1/8 w - - 0 1",
        "r1bqkb1r/pppppppp/2n2n2/8/8/2N2N2/PPPPPPPP/R1BQK2R w KQkq - 0 1",
        "r1bqkb1r/pppppppp/2n2n2/8/3P4/2N5/PPP1PPPP/R1BQK1NR w KQkq - 0 1",
        "rnbqkb1r/pppppppp/5n2/8/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 0 1",
        "r1bqkbnr/pppppppp/2n5/8/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 0 1",
        "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1",
        "r1bqk2r/pp2nppp/2np1n2/3p4/2PPp3/2N1PN2/PPP1B1PP/R1BQK2R w KQkq - 0 1",
        "8/PPPPPPPk/8/8/8/8/6K1/8 w - - 0 1",
    ];
}
// arrays for looping over pieces & colour
pub const PIECE_ID_TO_FEN: [char; 6] = ['p', 'n', 'b', 'r', 'q', 'k'];
pub const PIECE_KINDS: [PieceKind; 6] = [
    PieceKind::Pawn,
    PieceKind::Knight,
    PieceKind::Bishop,
    PieceKind::Rook,
    PieceKind::Queen,
    PieceKind::King,
];
pub const SLIDERS: [PieceKind; 3] = [PieceKind::Bishop, PieceKind::Rook, PieceKind::Queen];

pub const PIECE_COLOURS: [PieceColour; 2] = [PieceColour::White, PieceColour::Black];

pub const LIGHT_SQUARES: u64 = 0x55AA55AA55AA55AAu64;
pub const DARK_SQUARES: u64 = 0xAA55AA55AA55AA55u64;

// relevant squares for castling
// (obstructible_squares[colour][side], vulnerable_squares[colour_side])
pub const CASTLING_SQUARES: ([[BitBoard; 2]; 2], [[BitBoard; 2]; 2]) = (
    // squares that have to be free for castling
    [[14, 96], [1008806316530991104, 6917529027641081856]],
    // squares that can't be attacked for castling
    [[28, 112], [2017612633061982208, 8070450532247928832]],
);

// pawn structure testing stuff
pub const FILES: [BitBoard; 8] = get_files();
pub const ADJACENT_FILES: [BitBoard; 8] = get_adjacent_files();

const fn get_files() -> [BitBoard; 8] {
    let a_file: BitBoard = 0x101010101010101;
    let mut files = [0u64; 8];
    let mut i = 0;
    while i < 8 {
        files[i] = a_file << i;
        i += 1
    }
    files
}

const fn get_adjacent_files() -> [BitBoard; 8] {
    // 2- or 3- file chunks to test for pawn isolation
    let mut channels = [0u64; 8];
    let mut i = 0;
    while i < 8 {
        match i {
            0 => channels[i] = FILES[1],
            1..=6 => channels[i] = FILES[i - 1] | FILES[i + 1],
            7 => channels[i] = FILES[6],
            _ => unreachable!(),
        }
        i += 1
    }
    channels
}

// piece square tables
#[rustfmt::skip]
pub mod psts {
        
    type Pst = [i32; 64];

    pub const PAWN_PSTS: [Pst; 2] = [
        [
            // white pawns
            0,   0,   0,   0,   0,   0,   0,   0,

            0,   0,   0,  -5,  -5,   0,   0,   0,

            0,   0,   0,   0,   0,   0,   0,   0,

            0,   0,   5,   10,  10,  5,   0,   0,

            0,   0,   10,  15,  15,  10,  0,   0,

            5,   5,   10,  15,  15,  10,  5,   5,

            5,   10,  15,  20,  20,  15,  10,  5,

            0,   0,   0,   0,   0,   0,   0,   0
        ],
        
        // black pawns
        [
            0,   0,   0,   0,   0,   0,   0,   0,

            5,   10,  15,  20,  20,  15,  10,   5,

            5,   5,   10,  15,  15,  10,  5,   5,

            0,   0,   10,  15,  15,  10,   0,   0,

            0,   0,   5,   10,  10,  5,   0,   0,

            0,   0,   0,   0,   0,   0,   0,   0,

            0,   0,   0,  -5,  -5,   0,   0,   0,

            0,   0,   0,   0,   0,   0,   0,   0
        ],
    ];

    pub const KNIGHT_PST: Pst = [
        -20,  -10,  -10, -5,   -5,  -10, -10,  -20,

        -10,   0,    5,   10,   10,  5,   0,   -10,

        -10,   5,    15,  20,   20,  15,  5,   -10,

        -5,    10,   20,  25,   25,  20,  10,  -5,

        -5,    10,   20,  25,   25,  20,  10,  -5,

        -10,   5,    15,  20,   20,  15,  5,   -10,

        -10,   0,    5,   10,   10,  5,   0,   -10,

        -20,  -10,  -10, -5,   -5,  -10, -10,  -20,
    ];

    pub const BISHOP_PST: Pst = [
        -5,   0,   0,   0,   0,   0,   0,  -5,

         0,   5,   5,   5,   5,   5,   5,   0,

         0,   5,   10,  10,  10,  10,  5,   0,

         0,   5,   10,  15,  15,  10,  5,   0,

         0,   5,   10,  15,  15,  10,  5,   0,

         0,   5,   10,  10,  10,  10,  5,   0,

         0,   5,   5,   5,   5,   5,   5,   0,

        -5,   0,   0,   0,   0,   0,   0,  -5,
    ];

    pub const ROOK_PST: Pst = [
        0,   0,   0,   5,   5,   0,   0,   0,

        5,   5,   5,   5,   5,   5,   5,   5,

        0,   0,   0,   0,   0,   0,   0,   0,

        0,   0,   0,   0,   0,   0,   0,   0,

        0,   0,   0,   0,   0,   0,   0,   0,

        0,   0,   0,   0,   0,   0,   0,   0,

        5,   5,   5,   5,   5,   5,   5,   5,

        0,   0,   0,   5,   5,   0,   0,   0,

    ];

    pub const QUEEN_PST: Pst = [
        -10, -5,  -5,   0,   0,  -5,  -5,  -10,

        -5,   0,   5,   5,   5,   5,   0,  -5,

        -5,   5,   10,  10,  10,  10,  5,  -5,

         0,   5,   10,  10,  10,  10,  5,   0,

         0,   5,   10,  10,  10,  10,  5,   0,

        -5,   5,   10,  10,  10,  10,  5,  -5,

        -5,   0,   5,   5,   5,   5,   0,  -5,

        -10, -5,  -5,   0,   0,  -5,  -5,  -10,
    ];

    pub const KING_PST: Pst = [
        20,  30,  10,  0,   0,   10,  30,  20,

        20,  20,  0,  -10, -10,  0,   20,  20,

       -10, -20, -30, -30, -30, -30, -20, -10,

       -20, -30, -40, -40, -40, -40, -30, -20,

       -20, -30, -40, -40, -40, -40, -30, -20,

       -10, -20, -30, -30, -30, -30, -20, -10,

        20,  20,  0,   0,   0,   0,   20,  20,
        
        20,  30,  10,  0,   0,   10,  30,  20,
    ];
}
