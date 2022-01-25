{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.List (isPrefixOf)
import Domain
import Game
import Parser
import Test.Hspec
import Test.QuickCheck

parserSpec :: Spec
parserSpec = describe "parser" $ do
  it "can parse the whole deck" $ do
    parseInput "" (displayCards fullDeck) `shouldBe` Right fullDeck

displaySpec :: Spec
displaySpec = describe "displayGameResult" $ do
  it "should match the specified output" $ do
    displayGameResult
      GameFinished
        { winner = Sam,
          cardsSam = [Card Clubs Ace, Card Hearts Nine],
          cardsDealer = [Card Diamonds Five, Card Hearts Queen, Card Spades Eight]
        }
      `shouldBe` "sam\n"
        ++ "sam: CA, H9\n"
        ++ "dealer: D5, HQ, S8"

gameRuleSpec :: Spec
gameRuleSpec = do
  describe "Game" $ do
    describe "dealCards" $ do
      it "deal the cards in the order [sam, dealer, sam, dealer]" $
        do
          dealCards
            GameState
              { handSam = [],
                handDealer = [],
                deck = [Card Clubs Jack, Card Hearts Queen, Card Clubs King, Card Hearts Ace]
              }
          `shouldBe` Right
            GameState
              { handSam = [Card Clubs Jack, Card Clubs King],
                handDealer = [Card Hearts Queen, Card Hearts Ace],
                deck = []
              }
    describe "checkInitialHands" $ do
      it "sam wins if he has Blackjack in his initial hand" $
        do
          checkInitialHands
            GameState
              { handSam = [Card Clubs Jack, Card Clubs Ace],
                handDealer = [Card Hearts Queen, Card Hearts Two],
                deck = []
              }
          `shouldBe` Left
            GameFinished
              { winner = Sam,
                cardsSam = [Card Clubs Jack, Card Clubs Ace],
                cardsDealer = [Card Hearts Queen, Card Hearts Two]
              }
      it "dealer wins if she has Blackjack in her initial hand" $
        do
          checkInitialHands
            GameState
              { handSam = [Card Hearts Jack, Card Diamonds Four],
                handDealer = [Card Clubs Jack, Card Diamonds Ace],
                deck = []
              }
          `shouldBe` Left
            GameFinished
              { winner = Dealer,
                cardsSam = [Card Hearts Jack, Card Diamonds Four],
                cardsDealer = [Card Clubs Jack, Card Diamonds Ace]
              }
      it "sam wins if both have Blackjack in their initial hand" $
        do
          checkInitialHands
            GameState
              { handSam = [Card Hearts Ace, Card Clubs Jack],
                handDealer = [Card Spades Jack, Card Diamonds Ace],
                deck = []
              }
          `shouldBe` Left
            GameFinished
              { winner = Sam,
                cardsSam = [Card Hearts Ace, Card Clubs Jack],
                cardsDealer = [Card Spades Jack, Card Diamonds Ace]
              }
      it "dealer wins if both have a score of 22 in their initial hand" $
        do
          checkInitialHands
            GameState
              { handSam = [Card Hearts Ace, Card Clubs Ace],
                handDealer = [Card Spades Ace, Card Diamonds Ace],
                deck = []
              }
          `shouldBe` Left
            GameFinished
              { winner = Dealer,
                cardsSam = [Card Hearts Ace, Card Clubs Ace],
                cardsDealer = [Card Spades Ace, Card Diamonds Ace]
              }
    describe "samDraws" $ do
      it "if neither player has Blackjack then sam can start drawing cards from the top of the deck" $
        do
          samDraws
            GameState
              { handSam = [Card Hearts Ace, Card Clubs Five],
                handDealer = [Card Diamonds Ten, Card Spades Ten],
                deck = [Card Spades Seven]
              }
          `shouldBe` Right
            GameState
              { handSam = [Card Hearts Ace, Card Clubs Five, Card Spades Seven],
                handDealer = [Card Diamonds Ten, Card Spades Ten],
                deck = []
              }
      it "Sam can draw multiple Cards as long as his score is below 17" $
        do
          samDraws
            GameState
              { handSam = [Card Hearts Three, Card Clubs Five],
                handDealer = [Card Diamonds Ten, Card Spades Ten],
                deck = [Card Hearts Two, Card Clubs Two, Card Spades Two, Card Diamonds Seven]
              }
          `shouldBe` Right
            GameState
              { handSam = [Card Hearts Three, Card Clubs Five, Card Hearts Two, Card Clubs Two, Card Spades Two, Card Diamonds Seven],
                handDealer = [Card Diamonds Ten, Card Spades Ten],
                deck = []
              }
      it "sam must stop drawing cards from the deck if their total reaches 17 or higher" $
        do
          samDraws
            GameState
              { handSam = [Card Hearts Ace, Card Diamonds Seven],
                handDealer = [Card Clubs Ten, Card Spades Ten],
                deck = [Card Spades Five]
              }
          `shouldBe` Right
            GameState
              { handSam = [Card Hearts Ace, Card Diamonds Seven],
                handDealer = [Card Clubs Ten, Card Spades Ten],
                deck = [Card Spades Five]
              }
    describe "checkIfSamLost" $
      it "sam has lost the game if their total is higher than 21" $
        do
          checkIfSamLost
            GameState
              { handSam = [Card Hearts Ace, Card Clubs Five, Card Spades Seven],
                handDealer = [Card Diamonds Ten, Card Spades Ten],
                deck = [Card Spades Five]
              }
          `shouldBe` Left
            GameFinished
              { winner = Dealer,
                cardsSam = [Card Hearts Ace, Card Clubs Five, Card Spades Seven],
                cardsDealer = [Card Diamonds Ten, Card Spades Ten]
              }
    describe "dealerDraws" $ do
      it "when sam has stopped drawing cards the dealer can start drawing cards from the top of the deck" $
        do
          dealerDraws
            GameState
              { handSam = [Card Hearts Ace, Card Diamonds Seven],
                handDealer = [Card Clubs Ten, Card Spades Four],
                deck = [Card Spades Five]
              }
          `shouldBe` Right
            GameState
              { handSam = [Card Hearts Ace, Card Diamonds Seven],
                handDealer = [Card Clubs Ten, Card Spades Four, Card Spades Five],
                deck = []
              }
      it "the dealer must stop drawing cards when their total is higher than sam." $
        do
          dealerDraws
            GameState
              { handSam = [Card Hearts Ace, Card Diamonds Seven],
                handDealer = [Card Clubs Ten, Card Spades Ten],
                deck = [Card Spades Five]
              }
          `shouldBe` Right
            GameState
              { handSam = [Card Hearts Ace, Card Diamonds Seven],
                handDealer = [Card Clubs Ten, Card Spades Ten],
                deck = [Card Spades Five]
              }
    describe "checkIfDealerLost" $
      it "the dealer has lost the game if their total is higher than 21" $
        do
          checkIfDealerLost
            GameState
              { handSam = [Card Diamonds Ten, Card Spades Ten],
                handDealer = [Card Hearts Ace, Card Clubs Five, Card Spades Seven],
                deck = []
              }
          `shouldBe` Left
            GameFinished
              { winner = Sam,
                cardsSam = [Card Diamonds Ten, Card Spades Ten],
                cardsDealer = [Card Hearts Ace, Card Clubs Five, Card Spades Seven]
              }
    describe "determineWinner" $
      it "determines which player wins the game (highest score wins)" $
        do
          determineWinner
            ( Right
                GameState
                  { handSam = [Card Diamonds Ten, Card Spades Ten],
                    handDealer = [Card Hearts Ace, Card Clubs Five, Card Spades Five],
                    deck = []
                  }
            )
          `shouldBe` GameFinished
            { winner = Dealer,
              cardsSam = [Card Diamonds Ten, Card Spades Ten],
              cardsDealer = [Card Hearts Ace, Card Clubs Five, Card Spades Five]
            }
    it "Example from specification" $
      do
        let result = play [Card Clubs Ace, Card Diamonds Five, Card Hearts Nine, Card Hearts Queen, Card Spades Eight]
        result
          `shouldBe` GameFinished
            { winner = Sam,
              cardsSam = [Card Clubs Ace, Card Hearts Nine],
              cardsDealer = [Card Diamonds Five, Card Hearts Queen, Card Spades Eight]
            }
    it "Edge case: not enough cards for dealing" $
      do
        let result = play [Card Hearts Ace, Card Diamonds Two, Card Clubs Seven]
        result `shouldBe` NotEnoughCards
    it "Edge case: not enough cards for Sam" $
      do
        let result = play [Card Hearts Ace, Card Diamonds Seven, Card Clubs Two, Card Spades Nine]
        result `shouldBe` NotEnoughCards
    it "Edge case: not enough cards for dealer" $
      do
        let result = play [Card Hearts Ace, Card Diamonds Two, Card Clubs Seven, Card Spades Nine]
        result `shouldBe` NotEnoughCards
    it "Edge case: Sam can loose although dealer has an initial hand of score 22" $
      do
        let result = play [Card Hearts Ace, Card Diamonds Ace, Card Clubs Four, Card Spades Ace, Card Spades Nine]
        winner result `shouldBe` Dealer

newtype Deck = Deck [Card] deriving (Show)

-- consider only complete decks, so we alwasy have a winner
-- the edge cases with too few cards are already handled in the unit tests
instance Arbitrary Deck where
  arbitrary = Deck <$> shuffle fullDeck

-- Cards are dealt in the correct order
prop_CardOrder :: Deck -> Bool
prop_CardOrder (Deck cards) =
  let GameFinished {cardsDealer = d1 : d2 : dRest, cardsSam = s1 : s2 : sRest} = play cards
   in (s1 : d1 : s2 : d2 : sRest ++ dRest) `isPrefixOf` cards

-- before Sam's last draw, his hand should have a score below 17
-- this is trivially true, if Sam didn't take any card
prop_SamStopsAt17 :: Deck -> Bool
prop_SamStopsAt17 (Deck cards) =
  let result = play cards
   in score (init $ cardsSam result) < 17

-- If Sam's score is greater than 21 he loses
-- this is also the case in the initial phase, even when the dealer has a score of 22
prop_SamLosesWithScoreHigherThan21 :: Deck -> Bool
prop_SamLosesWithScoreHigherThan21 (Deck cards) =
  let GameFinished {..} = play cards
   in not (score cardsSam > 21 && winner == Sam)

-- If the dealer took an additional card, her score must have been not more than Sam's
prop_DealerStopsHigherThanSam :: Deck -> Bool
prop_DealerStopsHigherThanSam (Deck cards) =
  let GameFinished {..} = play cards
   in length cardsDealer == 2 || score (init cardsDealer) <= score cardsSam

gamePropertySpec :: Spec
gamePropertySpec =
  describe "play" $ do
    it "cards are dealt in the correct order" $
      property prop_CardOrder
    it "sam must stop drawing cards from the deck if their total reaches 17 or higher" $
      property prop_SamStopsAt17
    it "sam has lost the game if their total is higher than 21" $
      property prop_SamLosesWithScoreHigherThan21
    it "the dealer must stop drawing cards when their total is higher than sam" $
      property prop_DealerStopsHigherThanSam

main :: IO ()
main = hspec $ do
  displaySpec
  parserSpec
  gameRuleSpec
  gamePropertySpec
