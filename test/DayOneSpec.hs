module DayOneSpec where

import Data.Either (isLeft)
import DayOne
import Parsing
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "Parsing" $ do
    it "can handle one written digit" $ do
      parse writtenDigit "..." "five" `shouldBe` Right 5
    it "can handle many written digit" $ do
      parse (writtenDigits <* takeRest) "..." "eightwo" `shouldBe` Right [8, 2]
    it "fails with non written digit" $ do
      parse writtenDigit "..." "goon" `shouldSatisfy` isLeft
  describe "line" $ do
    it "handles simplest single line" $ do
      parse line "..." "eight" `shouldBe` Right [8]
    it "handles simple compound line" $ do
      parse line "..." "eight2" `shouldBe` Right [8, 2]
    it "handles truncated compound written line" $ do
      parse line "..." "eightwo" `shouldBe` Right [8, 2]
    it "handles compound written line" $ do
      parse line "..." "eighttwo" `shouldBe` Right [8, 2]
