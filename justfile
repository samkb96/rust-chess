default: help

help:
    @just --list

# human vs. bot mode; pass in the bot name and the colour for the bot to play (defaults to black if not provided)
human *args:
    cargo run --release --bin gui -- human {{args}}

# bot vs bot single game mode, rendered in the gui. pass in white bot selection, then black bot
bvb *args:
    cargo run --release --bin gui -- bvb {{args}}

# bot arena mode - plays 1000 bot games and records the results for version comparison. pass white, then black
arena *args:
    cargo run --release --bin cli -- arena {{args}}

# perft mode for legality checks. pass in start fen then depth
perft *args:
    cargo run --release --bin cli -- perft {{args}}

# default perft from usual start position, with all the base compiler optimisations. last check: 4.8m nps from first fen
movegen:
    # start
    cargo run --release --bin cli -- perft rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 6
    # middlegame
    cargo run --release --bin cli -- perft r1bqkb1r/pppp1ppp/2n2n2/4p3/3PP3/2N2N2/PPP2PPP/R1BQKB1R w KQkq - 1 8 5
    # endgame
    cargo run --release --bin cli -- perft 8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1 7

# perft without sliders. last check: 9.2m nps
movegen_nosliders:
    cargo run --release --bin cli -- perft 1n2k1n1/pppppppp/8/8/8/8/PPPPPPPP/1N2K1N1 w - - 0 1 6

# profiling - bot thinking
engine_profile *args:
    RUSTFLAGS="-C inline-threshold=0" cargo build --bin cli --release
    samply record cargo run  --bin cli --profile release -- arena {{args}}

# profiling - pure move generation
movegen_profile:
    RUSTFLAGS="-C inline-threshold=0" cargo build --bin cli --release
    samply record cargo run --bin cli --profile release -- perft rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 6

# profiling - pure move generation
movegen_profile_ni:
    samply record cargo run --bin cli --profile release -- perft rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 6

# magic bitboard generation
magic:
    cargo run --release --bin magic
fmt:
    cargo fmt

clippy:
    bacon clippy

