# Twentyone

This small application simulates a game of TwentyOne. It reads the shuffled deck from a file in the format

```
C2, D4, SJ, HA ...
```

or uses a randomly shuffled deck when no file is given. It will print the winner and the hands of the player and the dealer in the form

```
dealer
sam: S3, S5, C6, DA
dealer: D9, S7
```

## Building and Running

The project is written in Haskell based on Cabal, and also brings a nix build configuration.

### Nix

On NixOS or with the nix package manager installed you can build the executable with

```
nix build
```

and run it with

```
./result/bin/twentyone <path to input-file>
```

You can also use `nix-shell` inside the project directory to get a development shell with the correct versions of ghc and cabal in the PATH.

### Cabal

On a non nix system, the project can be build with


```
cabal build
```
and executed with

```
cabal run twentyone <path to input-file>
```

The test suite can be run with
```
cabal test
```

I have only tested the project with GHC 8.10.7 and Cabal 3.6.2.0

## Implementation notes

The application consists of the following modules:

- In the project library under `./src`
  - `Domain` contains the types for the game and a few  helper function to pretty print values
  - `Parser` is used to read a shuffled deck from a file. It is based on the `megaparsec` library
  - `Game` contains the logic for the game.

- In the executable sources under `./app`
  - `Main` connects the pure functions of the library with the `IO` functions to get command-line arguments, read the input-file and print the result.

- In the directory `./test`
  - `Test` contains unit tests based on `hspec` and a few property-based tess based on `QuickCheck`

I tried to match the implementation of the game rules closely to the structure of the specification. I extracted each step in the game spec into a separate function, and  use the `Either` monad to model the game flow, to accomodate the fact that a game can end from almost any step. In the type `Either GameResult GameState` a `Right state` represents a running game while a `Left result` is the result of a finished game. This allows the `game` method to have a linear structure, compared to nested `if/case` statements in a straightforward implementation:

```
play :: [Card] -> GameResult
play deck =
  let initialState = GameState {handSam = [], handDealer = [], deck = deck}
   in Right initialState
        >>= dealCards
        >>= checkInitialHands
        >>= samDraws
        >>= checkIfSamLost
        >>= dealerDraws
        >>= checkIfDealerLost
        & determineWinner
```

### Assumptions & design decisions

A few parts of the specification were not completely clear. These are the assumptions that I based my implementation on:

- Sam's strategy is to draw more cards until his hand scores at least 17 points.
- I assume that the input files can contain less than 52 cards (the example in the spec had only 5 cards). With an incomplete deck as input, it is possible that there are not enough cards on the desk to play a regular game. To handle this case I added an `NotEnoughCards` alternative to the `GameResult` type.
