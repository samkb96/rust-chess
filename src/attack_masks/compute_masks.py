#import random

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ATTACK MASKS AND DIRECTIONAL RAYS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# precompute all piece moves from each square for fast lookup

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

def generate_attack_masks():
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

    return {
        'white_pawn_attacks': create_attack_boards(white_pawn_directions),
        'black_pawn_attacks': create_attack_boards(black_pawn_directions),
        'knight_attacks': create_attack_boards(knight_directions),
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

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PARTIAL MASKS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# precompute masks between pieces to avoid looping over squares on rays in pin/check logic

def squares_list_to_bitboard(squares: list[int]):
    return sum(1 << square for square in squares)

def generate_directional_mask_between_inclusive(start: int, end: int, direction: tuple):

    direction_rank = direction[0]
    direction_file = direction[1]

    rank = start // 8
    file = start % 8

    squares = []

    while True:

        rank += direction_rank
        file += direction_file

        if (0 <= file < 8) and (0 <= rank < 8):

            new_square = 8 * rank + file
            squares.append(new_square)

            if new_square == end:
                return squares_list_to_bitboard(squares)

        else:
            return 0

def generate_mask_between_inclusive(start: int, end: int):
    directions = [(1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]

    if start == end:
        return 0

    for direction in directions:
        trial_mask = generate_directional_mask_between_inclusive(start, end, direction)

        if trial_mask > 0:
            return trial_mask

    return 0

def generate_partial_masks():
    inclusive_mask_dict = {}
    exclusive_mask_dict = {}

    for start in range(64):
        inclusive_mask_dict[start] = {}
        exclusive_mask_dict[start] = {}

        for end in range(64):
            mask = generate_mask_between_inclusive(start, end)

            inclusive_mask_dict[start][end] = mask
            exclusive_mask_dict[start][end] = mask ^ (1 << end) if mask > 0 else 0

    return inclusive_mask_dict, exclusive_mask_dict

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FILE WRITING ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# format outputs for lookup in masks.rs mod

def write_attack_masks(filename):

    attacks_dict = generate_attack_masks()
    inclusive_partial_masks, exclusive_partial_masks = generate_partial_masks()

    def format_array(const_name, array):

        lines = [f'pub const {const_name.upper()}: [BitBoard; 64] = [']

        for i in range(0, 64, 8):

            newboard = ', '.join(str(bitboard) for bitboard in array[i: i + 8])
            lines.append(f'  {newboard},')

        lines.append('];\n\n')
        return '\n'.join(lines)

    def format_double_array(const_name, array):

        lines = [f'\n\npub const {const_name.upper()}: [[BitBoard; 64]; 64] = [']

        for start, end_array in array.items():

            lines.append('    [')

            for i in range(0, 64, 8):

                end_array_rank_slice = list(end_array.values())[i: i+8]
                rank_masks_string = ', '.join(str(bitboard) for bitboard in end_array_rank_slice)
                lines.append(f'        {rank_masks_string},')

            lines.append('    ],')
        lines.append('];')
        return '\n'.join(lines)
    
    with open(filename, 'w') as f:
        f.write('use crate::mechanics::*;\n\n')
        f.write('// attack masks\n\n')
        for k, v in attacks_dict.items():
            f.write(format_array(k, v))
        f.write('\n\n// inclusive partial masks\n\n')
        f.write(format_double_array('MASK_UP_TO_INCLUSIVE', inclusive_partial_masks))

write_attack_masks('src/attack_masks/masks.rs')