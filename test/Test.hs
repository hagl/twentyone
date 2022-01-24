module Main (main) where

import Domain
import Parser
import Test.Hspec

parserSpec :: Spec
parserSpec = describe "parser" $ do
  it "can parse the whole deck" $ do
    parseInput "" (displayCards sortedDeck) `shouldBe` Right sortedDeck

main :: IO ()
main = hspec $ do
  parserSpec
