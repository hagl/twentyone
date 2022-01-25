{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Game where

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Set as Set
import Domain

score :: [Card] -> Int
score = sum . map cardValue

resultWithWinner :: Participant -> GameState -> GameResult
resultWithWinner p GameState {..} = GameFinished {winner = p, cardsSam = handSam, cardsDealer = handDealer}

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
    _ -> Right state -- no winner yet

-- take cards until score is over 17
playSam :: GameState -> Either GameResult GameState
playSam state@GameState {handSam} | score handSam >= 17 = Right state
playSam state@GameState {handSam, deck = c : cs} = playSam $ state {handSam = handSam ++ [c], deck = cs}
playSam _ = Left NotEnoughCards

didSamLose :: GameState -> Either GameResult GameState
didSamLose state =
  if score (handSam state) > 21 then Left $ resultWithWinner Dealer state else Right state

-- take cards until score is over 17
playDealer :: GameState -> Either GameResult GameState
playDealer state@GameState {..} | score handDealer > score handSam = Right state
playDealer state@GameState {handDealer, deck = c : cs} = playDealer $ state {handDealer = handDealer ++ [c], deck = cs}
playDealer _ = Left NotEnoughCards

finalResult :: Either GameResult GameState -> GameResult
finalResult (Left result) = result
finalResult (Right state@GameState {..}) =
  let scoreSam = score handSam
      scoreDealer = score handDealer
      winner = if scoreDealer <= 21 && scoreDealer > scoreSam || scoreSam > 21 then Dealer else Sam
   in resultWithWinner winner state

play :: [Card] -> GameResult
play deck =
  let initialState = GameState {handSam = [], handDealer = [], deck = deck}
   in Right initialState
        >>= dealCards
        >>= checkInitialHands
        >>= playSam
        >>= didSamLose
        >>= playDealer
        & finalResult
