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

# perft mode for legality checks / move generation testing. pass in start fen then depth
perft *args:
    cargo run --release --bin cli -- perft {{args}}

fmt:
    cargo fmt

clippy:
    bacon clippy