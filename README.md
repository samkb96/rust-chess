# rust-chess

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
- [x] Promotion picker
- [ ] (On hold) Sound effects in Ableton

### Rules and Moves
- [x] Bitboard interfacing
- [x] Pseudolegal move generation
- [x] Move validation, legal move highlights
- [x] Pin logic
- [x] Checking logic
- [x] Edge cases (castling, en passant)
- [x] Match node counts for a bunch of test positions
- [x] Game endings (50 move draw, repetition draw, insufficient material draw, stalemate, checkmate)
    - postponed repetition until zobrist hashing. going with max 400 move games instead for now
- [x] Time controls

### Bot v1
- [x] Simple evaluation function: material evaluation, piece-square tables
- [x] Simple search: negamax, alpha-beta

### Analytics
- [x] Speed test metrics
- [ ] Search metrics
    - to improve, average depth, max depth, etc.
- [ ] Thought process logs

### Move Generation: Possible Optimisation
- [x] Magic bitboards
- [x] Attack tables
- [x] Partial mask tables
- [ ] Occupancy filtering
- [x] Move masking
- [ ] Incremental updates

### Search Algorithm: Possible Optimisations
- [ ] Move ordering (MMV-LVA)
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
