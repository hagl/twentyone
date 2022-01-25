module Main (main) where

import Domain
import Game
import Game (playGame)
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
      GameResult
        { winner = Sam,
          cardsSam = [Card Clubs Ace, Card Hearts Nine],
          cardsDealer = [Card Diamonds Five, Card Hearts Queen, Card Spades Eight]
        }
      `shouldBe` "sam\n"
        ++ "sam: CA, H9\n"
        ++ "dealer: D5, HQ, S8"

gameRuleSpec :: Spec
gameRuleSpec = describe "game" $ do
  it "sam wins if he has Blackjack in his initial hand" $
    do
      playGame [Card Hearts Ace, Card Spades Two, Card Clubs Jack, Card Diamonds Four]
      `shouldBe` GameResult Sam [Card Hearts Ace, Card Clubs Jack] [Card Spades Two, Card Diamonds Four]
  it "dealer wins if she has Blackjack in her initial hand" $
    do
      playGame [Card Hearts Ace, Card Clubs Jack, Card Diamonds Four, Card Diamonds Ace]
      `shouldBe` GameResult Dealer [Card Hearts Ace, Card Diamonds Four] [Card Clubs Jack, Card Diamonds Ace]
  it "sam wins if both have Blackjack in their initial hand" $
    do
      playGame [Card Hearts Ace, Card Spades Jack, Card Clubs Jack, Card Diamonds Ace]
      `shouldBe` GameResult Sam [Card Hearts Ace, Card Clubs Jack] [Card Spades Jack, Card Diamonds Ace]
  it "dealer wins if both have a score of 22 in their initial hand" $
    do
      playGame [Card Hearts Ace, Card Clubs Ace, Card Diamonds Ace, Card Spades Ace]
      `shouldBe` GameResult Dealer [Card Hearts Ace, Card Diamonds Ace] [Card Clubs Ace, Card Spades Ace]
  it "if neither player has Blackjack then sam can start drawing cards from the top of the deck" $
    do
      let result = playGame [Card Hearts Ace, Card Diamonds Ten, Card Clubs Five, Card Spades Ten, Card Spades Seven]
      cardsSam result `shouldBe` [Card Hearts Ace, Card Clubs Five, Card Spades Seven]
  it "sam must stop drawing cards from the deck if their total reaches 17 or higher" $
    do
      let result = playGame [Card Hearts Ace, Card Clubs Ten, Card Diamonds Seven, Card Spades Ten, Card Spades Five]
      cardsSam result `shouldBe` [Card Hearts Ace, Card Diamonds Seven]
  it "sam has lost the game if their total is higher than 21" $
    do
      let result = playGame [Card Hearts Ace, Card Diamonds Ten, Card Clubs Five, Card Spades Ten, Card Spades Seven]
      winner result `shouldBe` Dealer

main :: IO ()
main = hspec $ do
  displaySpec
  parserSpec
  gameRuleSpec
