module Main where

import Domain (Card (Card), displayCards, sortedDeck)
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
  putStrLn $ displayCards deck
