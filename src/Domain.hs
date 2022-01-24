{-# LANGUAGE RecordWildCards #-}

module Domain where

import Data.List (intercalate)

data Suit = Club | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Enum, Show)

displaySuit :: Suit -> String
displaySuit Club = "C"
displaySuit Diamonds = "D"
displaySuit Hearts = "H"
displaySuit Spades = "S"

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Show)

displayValue :: Value -> String
displayValue Two = "2"
displayValue Three = "3"
displayValue Four = "4"
displayValue Five = "5"
displayValue Six = "6"
displayValue Seven = "7"
displayValue Eight = "8"
displayValue Nine = "9"
displayValue Ten = "10"
displayValue Jack = "J"
displayValue Queen = "Q"
displayValue King = "K"
displayValue Ace = "A"

value :: Value -> Int
value Two = 2
value Three = 3
value Four = 4
value Five = 5
value Six = 6
value Seven = 7
value Eight = 8
value Nine = 9
value Ten = 10
value Jack = 10
value Queen = 10
value King = 10
value Ace = 11

data Card = Card Suit Value
  deriving (Eq, Ord, Show)

displayCard :: Card -> String
displayCard (Card s v) = displaySuit s ++ displayValue v

displayCards :: [Card] -> String
displayCards cards = intercalate ", " (map displayCard cards)

cardValue :: Card -> Int
cardValue (Card _ v) = value v

sortedDeck :: [Card]
sortedDeck = [Card s v | s <- [Club .. Spades], v <- [Two .. Ace]]

data Participant = Sam | Dealer
  deriving (Eq, Ord, Show)

displayParticipant :: Participant -> String
displayParticipant Sam = "sam"
displayParticipant Dealer = "dealer"

data GameState = GameState
  { handSam :: [Card],
    handDealer :: [Card],
    deck :: [Card]
  }
  deriving (Eq, Ord, Show)

data GameResult = GameResult
  { winner :: Participant,
    cardsSam :: [Card],
    cardsDealer :: [Card]
  }
  deriving (Eq, Ord, Show)

displayGameResult :: GameResult -> String
displayGameResult GameResult {..} =
  displayParticipant winner
    ++ "\nsam: "
    ++ displayCards cardsSam
    ++ "\ndealer: "
    ++ displayCards cardsDealer
