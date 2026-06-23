// EXPLAINER
//
// When generating slider moves, looping over each square in each direction until we hit a blocker is slow
// We instead want to just look up the bitboard of attacked squares
//
// For this, we'd need a massive lookup table of size 2^14 for all the possible blocker combinations, for each square: ~1GB
// Would also be incredibly slow - we're having to linear search through 2^13 configurations until we find a match to retrieve the index of the attack mask
//
// However, most blocker configurations give the same attack mask, since a blocker behind another isn't affecting anything
// In the worst case (rook in the centre), there are 144 possible attack masks - way fewer than the blocker combinations
//
// Key idea: if we had a fast function that
//     - takes a bitboard of blocker combinations as input
//     - outputs a relatively small index
//     - sends different blocker combinations modulo redundancy to different indices
// We could use this to hash the blocker combinations into a far smaller table, with O(1) lookups instead of linear search
//
// Such functions exist; they look like (blocker_bitboard * magic_number) >> shift_amount
// We have basically no idea why this works, what characterises good magic_numbers, just gotta brute force it
// Can optimise for maximum index size to shrink lookup table by trying different magics for a while
// Still tends to be around 100-300x redundant, as equivalent configurations modulo redundancy get different indices on average. But still super fast

use crate::constants::masks::*;
use crate::mechanics::{BitBoard, PieceKind, closest_blocker, pop_lsb};
use rand::Rng;
use std::sync::OnceLock;

pub static MAGIC_BITBOARDS: OnceLock<MagicBitboards> = OnceLock::new();

/// initialisation
pub fn generate_static_magics() -> &'static MagicBitboards {
    MAGIC_BITBOARDS.get_or_init(generate_magic_bitboards)
}

/// access attack masks via slider type, start square, and blocker configuration
pub fn lookup_magic(piece_kind: PieceKind, square_index: usize, blockers: BitBoard) -> BitBoard {
    match piece_kind {
        PieceKind::Bishop => lookup_magic_for_slider_type(Slider::Bishop, square_index, blockers),
        PieceKind::Rook => lookup_magic_for_slider_type(Slider::Rook, square_index, blockers),
        PieceKind::Queen => {
            let bishop_directions = TRIMMED_BISHOP_MASKS[square_index];
            let bishop_blockers = bishop_directions & blockers;
            let bishop_attacks =
                lookup_magic_for_slider_type(Slider::Bishop, square_index, bishop_blockers);

            let rook_directions = TRIMMED_ROOK_MASKS[square_index];
            let rook_blockers = rook_directions & blockers;
            let rook_attacks =
                lookup_magic_for_slider_type(Slider::Rook, square_index, rook_blockers);

            bishop_attacks | rook_attacks
        }
        _ => panic!("Can't lookup magic bitboards for a non-slider"),
    }
}

fn lookup_magic_for_slider_type(
    slider: Slider,
    square_index: usize,
    blockers: BitBoard,
) -> BitBoard {
    let unlocked = MAGIC_BITBOARDS
        .get()
        .expect("Magics not intiialised properly");
    unlocked[slider as usize][square_index].get_attacks_for_square(blockers)
}

pub struct MagicBitboard {
    magic_number: u64,
    shift: u32,
    attack_table: AttackTable,
}

impl MagicBitboard {
    fn get_attacks_for_square(&self, blockers: BitBoard) -> BitBoard {
        let magic_index = (self.magic_number.wrapping_mul(blockers) >> self.shift) as usize;
        self.attack_table[magic_index]
    }
}

pub type MagicBitboards = [[MagicBitboard; 64]; 2]; // magic[slider_kind][square_index]

#[derive(Debug, Copy, Clone)]
enum Slider {
    Bishop,
    Rook,
}

type BlockerCombinations = Vec<BitBoard>;
type AttackTable = Box<[BitBoard]>;
type BlockerAttackPair = (BitBoard, BitBoard);
type BlockerAttackPairs = Vec<(BitBoard, BitBoard)>;

fn generate_magic_bitboards() -> MagicBitboards {
    [
        std::array::from_fn(|square_index| find_magic(Slider::Bishop, square_index)),
        std::array::from_fn(|square_index| find_magic(Slider::Rook, square_index)),
    ]
}

fn find_magic(slider: Slider, square_index: usize) -> MagicBitboard {
    let blocker_mask = match slider {
        Slider::Bishop => TRIMMED_BISHOP_MASKS[square_index],
        Slider::Rook => TRIMMED_ROOK_MASKS[square_index],
    };

    let max_blocker_count = blocker_mask.count_ones();

    let shift = 64 - max_blocker_count;
    let blocker_attack_pairs = get_all_attacks(slider, square_index);
    let mut rng = rand::rng();
    let mut attempts = 0usize;
    let attempts_counter = (1usize << 24) - 1;

    loop {
        // bitand three randoms to get a sparse one - tends to be better
        let magic_number = rng.random::<u64>() & rng.random::<u64>() & rng.random::<u64>();
        let mut attack_table = vec![0u64; (1 << max_blocker_count) as usize];
        let mut magic_found = true;
        let mut max_magic_index = 0;

        for &(blockers, attacks) in &blocker_attack_pairs {
            let magic_index = (blockers.wrapping_mul(magic_number) >> shift) as usize;
            max_magic_index = max_magic_index.max(magic_index);

            if attack_table[magic_index] == 0 {
                // set it if it's not been set already
                attack_table[magic_index] = attacks;
            } else if attack_table[magic_index] != attacks {
                // collision - try another magic
                magic_found = false;
                break;
            }
        }
        attempts += 1;
        if (attempts & attempts_counter) == 0 {
            let billions = attempts / 1_000_000_000;
            let millions = (attempts / 1_000_000) % 1000;
            let thousands = (attempts / 1_000) % 1000;
            let ones = attempts % 1000;

            let attempts_formatted = if billions > 0 {
                format!("{billions},{millions},{thousands},{ones}")
            } else {
                format!("{millions},{thousands},{ones}")
            };
            println!("Still looking - {attempts_formatted} attempts so far");
        }
        if magic_found {
            return MagicBitboard {
                magic_number,
                shift,
                attack_table: attack_table.into_boxed_slice(),
            };
        }
    }
}

fn get_all_attacks(slider: Slider, square_index: usize) -> BlockerAttackPairs {
    generate_blocker_combinations_for_square(slider, square_index)
        .iter()
        .map(|&blockers| blockers_to_attacks(slider, square_index, blockers))
        .collect()
}

fn blockers_to_attacks(
    slider: Slider,
    square_index: usize,
    blockers: BitBoard,
) -> BlockerAttackPair {
    let directions = match slider {
        Slider::Rook => ROOK_DIRECTIONS,
        Slider::Bishop => BISHOP_DIRECTIONS,
    };

    let attacks = directions
        .iter()
        .map(|&direction| ray_up_to_blocker(direction, square_index, blockers))
        .fold(0u64, |acc, elt| acc | elt);

    (blockers, attacks)
}

fn generate_blocker_combinations_for_square(
    slider: Slider,
    square_index: usize,
) -> BlockerCombinations {
    let unblocked_mask = match slider {
        Slider::Rook => ROOK_MASKS[square_index],
        Slider::Bishop => BISHOP_MASKS[square_index],
    };

    let mut trimmed_mask = trim_mask(unblocked_mask, square_index);

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

fn ray_up_to_blocker(direction: usize, square_index: usize, blockers: BitBoard) -> BitBoard {
    let direction_ray = ATTACK_MASKS[direction][square_index];
    let blockers_on_ray = blockers & direction_ray;

    if blockers_on_ray != 0 {
        let blocker_index = closest_blocker(direction, blockers_on_ray);
        return MASK_UP_TO_INCLUSIVE[square_index][blocker_index];
    }

    direction_ray
}

// helpers

fn assign_bits_to_indices(number: u16, square_indices: &[usize]) -> u64 {
    square_indices
        .iter()
        .enumerate()
        // shift nth number bit to first pos, delete rest, shift single bit to square_index
        // puts a blocker in index position iff the bit is set
        .map(|(n, square_index)| (((number >> n) & 1) as u64) << square_index)
        .fold(0u64, |acc, elt| acc | elt)
}
