{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Game where

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Set as Set
import Domain

{--|
  Plays a game of Twentyone with the given deck.

  This is the main entry point into the Game module.
--}
play ::
  -- | The deck to be played
  [Card] ->
  GameResult
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

-- Steps in the game
-- These functins are only exported from the module for unit tests

dealCards :: GameState -> Either GameResult GameState
dealCards GameState {deck = s1 : d1 : s2 : d2 : rest} =
  Right GameState {handSam = [s1, s2], handDealer = [d1, d2], deck = rest}
dealCards _ = Left NotEnoughCards

checkInitialHands :: GameState -> Either GameResult GameState
checkInitialHands state@GameState {..} =
  case (score handSam, score handDealer) of
    (21, _) -> Left $ resultWithWinner Sam state
    (_, 21) -> Left $ resultWithWinner Dealer state
    (22, 22) -> Left $ resultWithWinner Dealer state
    _ -> Right state -- no winner yet, continue

-- take cards until score is over 17
samDraws :: GameState -> Either GameResult GameState
samDraws state@GameState {handSam} | score handSam >= 17 = Right state
samDraws state@GameState {handSam, deck = c : cs} = samDraws $ state {handSam = handSam ++ [c], deck = cs}
samDraws _ = Left NotEnoughCards

-- Sam loses if his hand scores more than 21
checkIfSamLost :: GameState -> Either GameResult GameState
checkIfSamLost state | score (handSam state) > 21 = Left $ resultWithWinner Dealer state
checkIfSamLost state = Right state

-- take cards until score is higher than Sam's
dealerDraws :: GameState -> Either GameResult GameState
dealerDraws state@GameState {..} | score handDealer > score handSam = Right state
dealerDraws state@GameState {handDealer, deck = c : cs} = dealerDraws $ state {handDealer = handDealer ++ [c], deck = cs}
dealerDraws _ = Left NotEnoughCards

-- The dealer loses if her hand scores more than 21
checkIfDealerLost :: GameState -> Either GameResult GameState
checkIfDealerLost state | score (handDealer state) > 21 = Left $ resultWithWinner Sam state
checkIfDealerLost state = Right state

determineWinner :: Either GameResult GameState -> GameResult
determineWinner (Left result) = result
determineWinner (Right state@GameState {..}) =
  let scoreSam = score handSam
      scoreDealer = score handDealer
      winner = if scoreDealer > scoreSam then Dealer else Sam
   in resultWithWinner winner state

-- Helper functions

score :: [Card] -> Int
score = sum . map cardValue

resultWithWinner :: Participant -> GameState -> GameResult
resultWithWinner p GameState {..} = GameFinished {winner = p, cardsSam = handSam, cardsDealer = handDealer}
