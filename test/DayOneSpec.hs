module DayOneSpec where

import Control.Monad (join)
import Data.Either (isLeft)
import DayOne
import Parsing
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "overlappableWrittenDigits" $ do
    it "overlapping" $ do
      parse overlappableWrittenDigits "..." "eightwo" `shouldBe` Right [8, 2]
    it "adjacent" $ do
      parse overlappableWrittenDigits "..." "eighttwo" `shouldBe` Right [8, 2]
    it "not overlapping" $ do
      parse overlappableWrittenDigits "..." "eight2" `shouldBe` Right [8]

  describe "line" $ do
    it "handles simplest single line" $ do
      parse line "..." "eight" `shouldBe` Right [8]

    it "handles simple compound line" $ do
      parse line "..." "eight2" `shouldBe` Right [8, 2]

    it "handles truncated compound written line" $ do
      parse line "..." "eightwo" `shouldBe` Right [8, 2]

    it "handles compound written line" $ do
      parse line "..." "eighttwo" `shouldBe` Right [8, 2]

    it "handles compound written line with mixed inputs" $ do
      parse line "..." "5eighttwoeightwo2" `shouldBe` Right [5, 8, 2, 8, 2, 2]
