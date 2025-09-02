# rust-chess

A hobby project for learning Rust. 

---

## Overview

Aims:
 - Game interface for one/two player chess
 - Bots to choose from (easy, positional, tactical, hard)
 - Fancy move generation + search optimisation 
 - Mix of neural evaluation and classical techniques, dependent on personality parameters

---

## To-do

### Game interface
- [x] Render board and pieces
- [x] Drag and drop to move
- [ ] (On hold) Sound effects in Ableton

### Rules and Moves
- [x] Bitboard interfacing
- [x] Pseudolegal move generation
- [x] Move validation, legal move highlights
- [x] Pin logic
- [ ] Checking logic
- [ ] Edge cases (castling, en passant)
- [ ] Final rule check: possible n-move position counts from selected test positions, compared with stockfish
- [ ] Game endings (50 move draw, repetition draw, insufficient material draw, stalemate, checkmate)
- [ ] Time controls

### Bot v1
- [ ] Simple evaluation function: material evaluation, piece-square tables
- [ ] Simple search: minimax, alpha-beta

### Analytics
- [ ] Speed test metrics
- [ ] Search metrics
- [ ] Thought process logs

### Move Generation: Possible Optimisation
- [ ] Attack tables
- [ ] Occupancy filtering
- [ ] Move masking
- [ ] Incremental updates
- [ ] MMV-LVA ordering

### Search Algorithm: Possible Optimisations
- [ ] Move ordering
- [ ] Quiescence search
- [ ] Iterative deepening
- [ ] Transposition tables
- [ ] Null move pruning
- [ ] Late move reductions
- [ ] Futility pruning
- [ ] Specialised extensions

### ML Evaluation
- [ ] Data preparation
- [ ] Model architecture
- [ ] Training
- [ ] Export to TorchScript
- [ ] Implementation in engine

### Finalisation
- [ ] Combine neural eval with traditional methods
- [ ] Incremental evaluation
- [ ] ELO calculation
- [ ] Interface options (model strength, time controls)
