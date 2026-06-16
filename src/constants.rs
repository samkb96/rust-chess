use crate::attack_masks::masks::*;
use crate::mechanics::*;

// fen strings for intialisation of position
pub const START_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

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
pub const PIECE_COLOURS: [PieceColour; 2] = [PieceColour::White, PieceColour::Black];

// attacks and directions
// 0-3 are increasing along the ray; 4-7 decreasing
pub const ATTACK_MASKS: [[BitBoard; 64]; 8] = [
    NORTHWEST_RAY,
    NORTH_RAY,
    NORTHEAST_RAY,
    EAST_RAY,
    SOUTHEAST_RAY,
    SOUTH_RAY,
    SOUTHWEST_RAY,
    WEST_RAY,
];
pub const PAWN_ATTACKS: [[BitBoard; 64]; 2] = [WHITE_PAWN_ATTACKS, BLACK_PAWN_ATTACKS];

pub const BISHOP_DIRECTIONS: &[usize] = &[0, 2, 4, 6];
pub const ROOK_DIRECTIONS: &[usize] = &[1, 3, 5, 7];
pub const QUEEN_DIRECTIONS: &[usize] = &[0, 1, 2, 3, 4, 5, 6, 7];

// relevant squares for castling
// (obstructible_squares[colour][side], vulnerable_squares[colour_side])
pub const CASTLING_SQUARES: ([[BitBoard; 2]; 2], [[BitBoard; 2]; 2]) = (
    // squares that have to be free for castling
    [[14, 96], [1008806316530991104, 6917529027641081856]],
    // squares that can't be attacked for castling
    [[28, 112], [2017612633061982208, 8070450532247928832]],
);

// piece square tables
#[rustfmt::skip]
pub mod psts {
        
    type Pst = [i32; 64];

    pub const PAWN_PSTS: [Pst; 2] = [
        [
            // white pawns
            0,   0,   0,   0,   0,   0,   0,   0,

            0,   0,   0,   0,   0,   0,   0,   0,

            0,   0,   0,   0,   0,   0,   0,   0,

            0,   0,   0,   0,   0,   0,   0,   0,

            0,   0,   5,   10,  10,  5,   0,   0,

            5,   5,   10,  15,  15,  10,  5,   5,

            5,   5,   5,   10,  10,  5,   5,   5,

            0,   0,   0,   0,   0,   0,   0,   0
        ],
        
        // black pawns
        [
            0,   0,   0,   0,   0,   0,   0,   0,

            5,   5,   5,   10,  10,  5,   5,   5,

            5,   5,   10,  15,  15,  10,  5,   5,

            0,   0,   5,   10,  10,  5,   0,   0,

            0,   0,   0,   0,   0,   0,   0,   0,

            0,   0,   0,   0,   0,   0,   0,   0,

            0,   0,   0,   0,   0,   0,   0,   0,

            0,   0,   0,   0,   0,   0,   0,   0
        ],
    ];

    pub const KNIGHT_PST: Pst = [
        -20,  -10,  -10,  -5,  -5,  -10, -10,  -20,

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
        0, 0, 0, 5, 5, 0, 0, 0,
        5, 5, 5, 5, 5, 5, 5, 5,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        5, 5, 5, 5, 5, 5, 5, 5,
        0, 0, 0, 5, 5, 0, 0, 0,
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

        20,  20,  0,   0,   0,   0,   20,  20,

       -10, -20, -30, -30, -30, -30, -20, -10,

       -20, -30, -40, -40, -40, -40, -30, -20,

       -20, -30, -40, -40, -40, -40, -30, -20,

       -10, -20, -30, -30, -30, -30, -20, -10,

        20,  20,  0,   0,   0,   0,   20,  20,
        
        20,  30,  10,  0,   0,   10,  30,  20,
    ];
}
