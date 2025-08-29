
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

north = [(1, 0)]
northeast = [(1, 1)]
east = [(0, 1)]
southeast = [(-1, 1)]
south = [(-1, 0)]
southwest = [(-1, -1)]
west = [(0, -1)]
northwest = [(1, -1)]

white_pawn_directions = northeast + northwest
black_pawn_directions = southeast + southwest
bishop_directions = white_pawn_directions + black_pawn_directions
rook_directions = north + east + south + west
king_directions = rook_directions + bishop_directions
knight_directions = [(2, 1), (1, 2), (2, -1), (1, -2), (-2, 1), (-1, 2), (-2, -1), (-1, -2)]

attacks_dict = {
    'white_pawn_attacks': create_attack_boards(white_pawn_directions),
    'black_pawn_attacks': create_attack_boards(black_pawn_directions),
    'knight_attacks': create_attack_boards(knight_directions),
    'bishop_attacks': create_attack_boards(bishop_directions, 8),
    'rook_attacks': create_attack_boards(rook_directions, 8),
    'queen_attacks': create_attack_boards(king_directions, 8),
    'king_attacks': create_attack_boards(king_directions),

    'north_ray': create_attack_boards(north, 8),
    'northeast_ray': create_attack_boards(northeast, 8),
    'east_ray': create_attack_boards(east, 8),
    'southeast_ray': create_attack_boards(southeast, 8),
    'south_ray': create_attack_boards(south, 8),
    'southwest_ray': create_attack_boards(southwest, 8),
    'west_ray': create_attack_boards(west, 8),
    'northwest_ray': create_attack_boards(northwest, 8)
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

# sanity check 
def print_bitboard(bb, start_rank, start_file):
    for rank in range(7, -1, -1):  
        row = []
        for file in range(8): 
            square_index = rank * 8 + file
            mask = 1 << square_index
            if (rank, file) == (start_rank, start_file):
                character = 'O'
            elif bb & mask:
                character = '1'
            else:
                character = '.'

            row.append(character)
        print(" ".join(row))
    print()

for (i, bb) in enumerate(attacks_dict['queen_attacks'][:16]):
    rank, file = divmod(i, 8)
    print(f'Attack mask for rank {rank + 1}, file {file + 1}:')
    print_bitboard(bb, rank, file)

write_attack_masks('src/attack_masks/masks.rs', attacks_dict)