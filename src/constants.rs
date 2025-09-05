use crate::mechanics::*;
use crate::attack_masks::masks::*;

// fen strings for intialisation of position
pub const INITIALISATION_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"; // at some point, rework the from_fen function to get rank order/colour right

// others for testing scenarios

pub const EN_PASSANT_TEST: &str = "6k1/8/8/K1Pp4/8/8/8/8 w - d6 0 1";
pub const EN_PASSANT_CHECK_TEST: &str = "6k1/8/8/K1Pp3q/8/8/8/8 w - d6 0 1";



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


