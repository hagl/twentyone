module Main where

import Domain (Card (Card), displayCards, displayGameResult, sortedDeck)
import Game (DeckError (..), play)
import Parser
import System.Environment
import System.Random.Shuffle

readDeck :: String -> IO [Card]
readDeck filename = do
  contents <- readFile filename
  let parsed = parseInput filename contents
  case parsed of
    Left parseError -> fail parseError
    Right result -> pure result

main :: IO ()
main = do
  a <- getArgs
  deck <-
    if null a
      then shuffleM sortedDeck
      else readDeck (head a)
  case play deck of
    Left DuplicateCards -> putStrLn "Input contains duplicate cards"
    Left IncompleteDeck -> putStrLn "Input is not a complete deck of cards"
    Right result -> putStrLn $ displayGameResult result
