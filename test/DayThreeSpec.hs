module DayThreeSpec where

import DayThree
import Test.Hspec

spec :: Spec
spec = do
  describe "toNumber" $ do
    it "works" $ do
      toNumber [4, 6, 7] `shouldBe` 467
    it "works" $ do
      toNumber [4, 0, 1] `shouldBe` 401
    it "works" $ do
      toNumber [] `shouldBe` 0
