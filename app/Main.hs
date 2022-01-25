module Main where

import Domain (Card (Card), displayCards, displayGameResult, fullDeck)
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
      then shuffleM fullDeck
      else readDeck (head a)
  case play deck of
    Left DuplicateCards -> putStrLn "Error: Input contains duplicate cards"
    Left IncompleteDeck -> putStrLn "Error: Input is not a complete deck of cards"
    Right result -> putStrLn $ displayGameResult result
