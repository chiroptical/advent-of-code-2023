module DayThreeSpec where

import DayThree
import Test.Hspec

spec :: Spec
spec = do
  let isPeriod = \case
        Period -> True
        _ -> False
  describe "toNumber" $ do
    it "works" $ do
      toNumber [4, 6, 7] `shouldBe` 467
    it "works" $ do
      toNumber [4, 0, 1] `shouldBe` 401
    it "works" $ do
      toNumber [] `shouldBe` 0
  describe "findWithStencil" $ do
    let sizedSparseMatrix =
          fromZipped
            2
            [ (0, [(0, Symbol '_'), (1, Period)])
            , (1, [(0, Period), (1, Period)])
            ]
    it "finds a period next to the symbol at 0, 0" $ do
      findWithStencil haloStencil isPeriod 0 0 sizedSparseMatrix `shouldBe` True
    it "can't find a symbol next to the symbol at 0, 0" $ do
      findWithStencil haloStencil isSymbol 0 0 sizedSparseMatrix `shouldBe` False
    it "can find a period next to the period at 0, 1" $ do
      findWithStencil haloStencil isPeriod 0 1 sizedSparseMatrix `shouldBe` True
    it "can find a symbol next to the period at 0, 1" $ do
      findWithStencil haloStencil isSymbol 0 1 sizedSparseMatrix `shouldBe` True
