module Main where

import Domain (Card (Card), displayCards, displayGameResult, fullDeck)
import Game (play)
import Parser
import System.Environment
import System.Random.Shuffle

readDeck :: String -> IO [Card]
readDeck filename = do
  contents <- readFile filename
  let parsed = parseInput filename contents
  case parsed of
    Left parseError -> fail $ "Deck cannot be parsed: " ++ parseError
    Right result -> pure result

main :: IO ()
main = do
  a <- getArgs
  deck <-
    if null a
      then shuffleM fullDeck
      else readDeck (head a)
  let result = play deck
  putStrLn $ displayGameResult result
