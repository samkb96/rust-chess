use bradybot::constants::masks::*;
use bradybot::mechanics::{BitBoard, pop_lsb};

use rand;

const A_FILE: BitBoard = 72340172838076673;
const H_FILE: BitBoard = 9259542123273814144;
const FIRST_RANK: BitBoard = 255;
const LAST_RANK: BitBoard = 18374686479671623680;

const EDGES: [BitBoard; 4] = [A_FILE, H_FILE, FIRST_RANK, LAST_RANK];

#[derive(Debug, Copy, Clone)]
enum Slider {
    Rook,
    Bishop,
}

fn main() {
    let rook_combinations = generate_blocker_combinations(Slider::Rook);
    let bishop_combinations = generate_blocker_combinations(Slider::Bishop);
}

fn generate_blocker_combinations(slider: Slider) -> Vec<Vec<BitBoard>> {
    (0..64)
        .map(|x| generate_blocker_combinations_for_square(slider, x))
        .collect()
}

fn generate_blocker_combinations_for_square(slider: Slider, index: usize) -> Vec<BitBoard> {
    let unblocked_mask = match slider {
        Slider::Rook => ROOK_MASKS[index],
        Slider::Bishop => BISHOP_MASKS[index],
    };

    let mut trimmed_mask = unblocked_mask;
    let mask_with_start_point = unblocked_mask | 1 << index;

    // trim the edge of the mask, since capturable enemy / empty are same thing from blocking pov
    for edge in EDGES {
        if mask_with_start_point & edge != edge {
            trimmed_mask &= !edge;
        } // if we're a rook pointing at the whole edge, don't trim 
    }

    // get the indices of all the squares in the trimmed mask
    let max_blocker_count = trimmed_mask.count_ones() as u16;
    let mut possible_blocker_indices = Vec::<usize>::with_capacity(max_blocker_count as usize);
    
    while let Some(blocker_index) = pop_lsb(&mut trimmed_mask) {
        possible_blocker_indices.push(blocker_index)
    }

    // take each number up to total combinations, shift the nth binary digit by the nth possible blocker index
    let total_combinations = 1u16 << max_blocker_count;

    (0..total_combinations)
        .map(|x| assign_bits_to_indices(x, &possible_blocker_indices))
        .collect()
}

fn assign_bits_to_indices(number: u16, square_indices: &[usize]) -> u64 {
    square_indices
        .iter()
        .enumerate()
        .map(|(n, square_index)| 
            // shift nth number bit to first pos, delete rest, shift single bit to square_index
            // puts a blocker in index position iff the bit is set
            (((number >> n) & 1) as u64) << square_index
        )
        .fold(0u64, |acc, elt| acc | elt)
}
