
def gen_moves(square, directions, repeats):
    origin_rank, origin_file = divmod(square, 8)
    moves = []
    for up, right in directions:
        rank, file = origin_rank, origin_file
        repeats_per_direction = repeats
        while repeats_per_direction > 0:
            rank += up
            file += right
            if (0 <= rank < 8) and (0 <= file < 8):
                moves.append(rank * 8 + file)
            repeats_per_direction -= 1
    return moves

def squares_to_bitboard(squares):
    bitboard = 0
    for square in squares:
        bitboard |= 1 << square
    return bitboard

def create_attack_boards(directions, repeats = 1):
    return [squares_to_bitboard(gen_moves(square, directions, repeats)) for square in range(64)]

white_pawn_directions = [(1, 1), (1, -1)]
black_pawn_directions = [(-1, 1), (-1, -1)]
knight_directions = [(2, 1), (1, 2), (2, -1), (1, -2), (-2, 1), (-1, 2), (-2, -1), (-1, -2)]
bishop_directions = white_pawn_directions + black_pawn_directions
rook_directions = [(1, 0), (0, 1), (-1, 0), (0, -1)]
king_directions = rook_directions + bishop_directions

attacks_dict = {
    'white_pawn_attacks': create_attack_boards(white_pawn_directions),
    'black_pawn_attacks': create_attack_boards(black_pawn_directions),
    'knight_attacks': create_attack_boards(knight_directions),
    'bishop_attacks': create_attack_boards(bishop_directions, 8),
    'rook_attacks': create_attack_boards(rook_directions, 8),
    'queen_attacks': create_attack_boards(king_directions, 8),
    'king_attacks': create_attack_boards(king_directions)
}

def write_attack_masks(filename, attacks_dict):
    def format_array(const_name, array):
        lines = [f'pub const {const_name.upper()}: [u64; 64] = [']
        for i in range(0, 64, 8):
            newboard = ', '.join(str(bitboard) for bitboard in array[i: i + 8])
            lines.append(f'  {newboard},')
        lines.append('];\n\n')
        return '\n'.join(lines)
    
    with open(filename, 'w') as f:
        f.write('// attack masks\n\n')
        for k, v in attacks_dict.items():
            f.write(format_array(k, v))

write_attack_masks('attack_masks/masks.rs', attacks_dict)