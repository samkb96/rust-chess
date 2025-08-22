# rust-chess

A hobby project for learning Rust. 

---

## Overview

Aims:
 - A game interface for single-player chess
 - A bot with a strength parameter to play against
 - Fancy move generation, search optimisation, neural evaluation 

---

## To-do

### Game interface
- [x] Render board and pieces
- [x] Drag and drop to move
- [ ] Sound effects in Ableton

### Rules and Moves
- [ ] Bitboard interfacing
- [ ] Encode rules, move generation
- [ ] Possible positions after n moves counter to test rule implementation
- [ ] Move validation, legal move highlights
- [ ] Implement game state (check, checkmate, stalemate)
- [ ] Time control

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
