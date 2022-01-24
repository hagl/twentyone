module Parser (parseInput) where

import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Domain

type Parser = Parsec Void String

suitParser :: Parser Suit
suitParser =
  choice
    [ char 'C' $> Club,
      char 'D' $> Diamonds,
      char 'H' $> Hearts,
      char 'S' $> Spades
    ]

valueParser :: Parser Value
valueParser =
  choice
    [ char '2' $> Two,
      char '3' $> Three,
      char '4' $> Four,
      char '5' $> Five,
      char '6' $> Six,
      char '7' $> Seven,
      char '8' $> Eight,
      char '9' $> Nine,
      string "10" $> Ten,
      char 'J' $> Jack,
      char 'Q' $> Queen,
      char 'K' $> King,
      char 'A' $> Ace
    ]

cardParser :: Parser Card
cardParser = Card <$> suitParser <*> valueParser

cardsParser :: Parser [Card]
cardsParser = sepBy cardParser (string ", ")

parseInput :: String -> String -> Either String [Card]
parseInput filename content =
  first errorBundlePretty $ parse (cardsParser <* optional eol <* eof) filename content
