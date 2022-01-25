{-# LANGUAGE RecordWildCards #-}

module Domain where

import Data.List (intercalate)

-- | The suit symbol of a card
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Enum, Show)

-- | Converts a Suite alternative to a single letter represntation used for output
displaySuit :: Suit -> String
displaySuit Clubs = "C"
displaySuit Diamonds = "D"
displaySuit Hearts = "H"
displaySuit Spades = "S"

-- | The value of a card
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Show)

-- | Converts a Value alternative to its numerical form, or a single letter represntation for face cards and aces
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

-- computes the int value of a card value for the score calculation
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

-- | A `Card` is determined by `Suit` and `Value`
data Card = Card Suit Value
  deriving (Eq, Ord, Show)

-- | Computes the short string representation of a `Card`
displayCard :: Card -> String
displayCard (Card s v) = displaySuit s ++ displayValue v

-- | Computes the short string representation of a `Card`
displayCards :: [Card] -> String
displayCards cards = intercalate ", " (map displayCard cards)

cardValue :: Card -> Int
cardValue (Card _ v) = value v

-- | All cards of a deck
fullDeck :: [Card]
fullDeck = [Card s v | s <- [Clubs .. Spades], v <- [Two .. Ace]]

-- | The participants in the game
data Participant = Sam | Dealer
  deriving (Eq, Ord, Show)

-- | The participants are printed in lower case
displayParticipant :: Participant -> String
displayParticipant Sam = "sam"
displayParticipant Dealer = "dealer"

-- | The state of a running game
data GameState = GameState
  { -- | the cards in Sam's hand
    handSam :: [Card],
    -- | the cards in the dealer's hand
    handDealer :: [Card],
    -- | the cards in the deck
    deck :: [Card]
  }
  deriving (Eq, Ord, Show)

data GameResult
  = -- | The game ended regularly
    GameFinished
      { -- | who won the game
        winner :: Participant,
        -- | the cards in Sam's hand
        cardsSam :: [Card],
        -- | the cards in the dealer's hand
        cardsDealer :: [Card]
      }
  | -- | The game could not proceed because the deck was empty.
    NotEnoughCards
  deriving (Eq, Show)

displayGameResult :: GameResult -> String
displayGameResult GameFinished {..} =
  displayParticipant winner
    ++ "\nsam: "
    ++ displayCards cardsSam
    ++ "\ndealer: "
    ++ displayCards cardsDealer
displayGameResult NotEnoughCards = "Error: There are not enough cards in the deck to finish the game"
