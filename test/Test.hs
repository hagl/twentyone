module Main (main) where

import Domain
import Game (play)
import Parser
import Test.Hspec

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
  describe "play" $ do
    it "sam wins if he has Blackjack in his initial hand" $
      do
        play [Card Hearts Ace, Card Spades Two, Card Clubs Jack, Card Diamonds Four]
        `shouldBe` GameFinished Sam [Card Hearts Ace, Card Clubs Jack] [Card Spades Two, Card Diamonds Four]
    it "dealer wins if she has Blackjack in her initial hand" $
      do
        play [Card Hearts Ace, Card Clubs Jack, Card Diamonds Four, Card Diamonds Ace]
        `shouldBe` GameFinished Dealer [Card Hearts Ace, Card Diamonds Four] [Card Clubs Jack, Card Diamonds Ace]
    it "sam wins if both have Blackjack in their initial hand" $
      do
        play [Card Hearts Ace, Card Spades Jack, Card Clubs Jack, Card Diamonds Ace]
        `shouldBe` GameFinished Sam [Card Hearts Ace, Card Clubs Jack] [Card Spades Jack, Card Diamonds Ace]
    it "dealer wins if both have a score of 22 in their initial hand" $
      do
        play [Card Hearts Ace, Card Clubs Ace, Card Diamonds Ace, Card Spades Ace]
        `shouldBe` GameFinished Dealer [Card Hearts Ace, Card Diamonds Ace] [Card Clubs Ace, Card Spades Ace]
    it "if neither player has Blackjack then sam can start drawing cards from the top of the deck" $
      do
        let result = play [Card Hearts Ace, Card Diamonds Ten, Card Clubs Five, Card Spades Ten, Card Spades Seven]
        cardsSam result `shouldBe` [Card Hearts Ace, Card Clubs Five, Card Spades Seven]
    it "sam must stop drawing cards from the deck if their total reaches 17 or higher" $
      do
        let result = play [Card Hearts Ace, Card Clubs Ten, Card Diamonds Seven, Card Spades Ten, Card Spades Five]
        cardsSam result `shouldBe` [Card Hearts Ace, Card Diamonds Seven]
    it "sam has lost the game if their total is higher than 21" $
      do
        let result = play [Card Hearts Ace, Card Diamonds Ten, Card Clubs Five, Card Spades Ten, Card Spades Seven]
        winner result `shouldBe` Dealer
    it "when sam has stopped drawing cards the dealer can start drawing cards from the top of the deck" $
      do
        let result = play [Card Hearts Ace, Card Diamonds Ten, Card Clubs Seven, Card Spades Six, Card Spades Seven]
        cardsDealer result `shouldBe` [Card Diamonds Ten, Card Spades Six, Card Spades Seven]
    it "the dealer must stop drawing cards when their total is higher than sam." $
      do
        let result = play [Card Hearts Ace, Card Diamonds Ten, Card Clubs Seven, Card Spades Nine, Card Spades Seven]
        cardsDealer result `shouldBe` [Card Diamonds Ten, Card Spades Nine]
    it "the dealer has lost the game if their total is higher than 21" $
      do
        let result = play [Card Hearts Ace, Card Diamonds Ten, Card Clubs Seven, Card Spades Six, Card Spades Seven]
        winner result `shouldBe` Sam
    it "determine which player wins the game (highest score wins)" $
      do
        let result = play [Card Hearts Ace, Card Diamonds Ten, Card Clubs Seven, Card Spades Nine]
        winner result `shouldBe` Dealer

main :: IO ()
main = hspec $ do
  displaySpec
  parserSpec
  gameRuleSpec
