{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Game (play, DeckError (..)) where

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Set as Set
import Domain

score :: [Card] -> Int
score = sum . map cardValue

resultWithWinner :: Participant -> GameState -> GameResult
resultWithWinner p GameState {..} = GameResult {winner = p, cardsSam = handSam, cardsDealer = handDealer}

dealCards :: GameState -> GameState
dealCards GameState {deck = s1 : d1 : s2 : d2 : rest} =
  GameState {handSam = [s1, s2], handDealer = [d1, d2], deck = rest}

checkInitialHands :: GameState -> Either GameResult GameState
checkInitialHands state@GameState {..} =
  case (score handSam, score handDealer) of
    (21, _) -> Left $ resultWithWinner Sam state
    (_, 21) -> Left $ resultWithWinner Dealer state
    (22, _) -> Left $ resultWithWinner Dealer state
    (_, 22) -> Left $ resultWithWinner Sam state
    _ -> Right state -- no winner yet

-- take cards until score is over 17
playSam :: GameState -> GameState
playSam state@GameState {..} =
  if score handSam >= 17
    then state
    else playSam $ state {handSam = handSam ++ [head deck], deck = tail deck}

didSamLose :: GameState -> Either GameResult GameState
didSamLose state =
  if score (handSam state) > 21 then Left $ resultWithWinner Dealer state else Right state

-- take cards until score is over 17
playDealer :: GameState -> GameState
playDealer state@GameState {..} =
  if score handDealer <= score handSam
    then playDealer $ state {handDealer = handDealer ++ [head deck], deck = tail deck}
    else state

finalResult :: Either GameResult GameState -> GameResult
finalResult (Left result) = result
finalResult (Right state@GameState {..}) =
  let scoreSam = score handSam
      scoreDealer = score handDealer
      winner = if scoreDealer <= 21 && scoreDealer > scoreSam || scoreSam > 21 then Dealer else Sam
   in resultWithWinner winner state

data DeckError = DuplicateCards | IncompleteDeck

play :: [Card] -> Either DeckError GameResult
play deck
  | length (Set.fromList deck) < length deck = Left DuplicateCards
  | length deck < length sortedDeck = Left IncompleteDeck
  | otherwise = Right $ playGame deck

playGame :: [Card] -> GameResult
playGame deck =
  let initialState = GameState {handSam = [], handDealer = [], deck = deck}
   in Right initialState
        <&> dealCards
        >>= checkInitialHands
        <&> playSam
        >>= didSamLose
        <&> playDealer
        & finalResult
