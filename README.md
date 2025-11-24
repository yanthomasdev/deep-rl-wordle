This is a college Haskell project that implements:

1. A terminal (CLI) version of the Wordle game (`wordle` package).
2. A Deep Q-Network (DQN) reinforcement learning agent that learns to play Wordle (`agent` package).
3. Shared utilities used by both (`shared` package).

## Project structure

```
deep-rl-wordle/
  agent/
    app/Main.hs        -- Training / demonstration entry point
    src/DQN.hs         -- DQN implementation (network, backprop, batching)
    agent.cabal
  wordle/
    app/Main.hs        -- Human-playable CLI
    src/Game.hs        -- Game mechanics
    wordle.cabal
  shared/
    src/Utils.hs       -- Shared utilities (word loading, formatting, types)
    shared.cabal
  guesses.txt          -- Valid non-solution guesses (optional for agent)
  solutions.txt        -- Canonical solution list (also used as agent vocab)
  wordle_dqn_best.txt  -- Best saved model (auto-written during training)
  wordle_dqn_last.txt  -- Last saved model
  cabal.project
  LICENSE
```

## Setup

Prerequisites:
- GHC (≥ 9.x recommended)
- Cabal (≥ 3.x)

Build packages:
```
cabal build all
```

Start training:
```
cabal run agent
```

Demonstrate trained model:
```
cabal run agent -- --demonstrate (optional: --load-last to load last saved model)
```

Play Wordle yourself:
```
cabal run wordle-cli
```
