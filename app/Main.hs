module Main where

import Domain (Card, displayGameResult, fullDeck)
import Game (play)
import Parser (parseInput)
import System.Environment (getArgs)
import System.Random.Shuffle (shuffleM)

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
