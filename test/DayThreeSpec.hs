module DayThreeSpec where

import Control.Monad.Trans.State
import DayThree
import SizedSparseMatrix qualified as Matrix
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
  describe "labelGears" $ do
    it "works" $ do
      let input =
            Matrix.fromList
              [ ((0, 0), Symbol '*')
              , ((0, 1), Number 0)
              , ((1, 0), Symbol '_')
              , ((1, 1), Symbol '*')
              , ((2, 0), Period)
              , ((2, 1), Number 1)
              ]
          output =
            Matrix.fromList
              [ ((0, 0), Gear 1)
              , ((0, 1), PartNumber 0)
              , ((1, 0), Neither)
              , ((1, 1), Gear 2)
              , ((2, 0), Neither)
              , ((2, 1), PartNumber 1)
              ]
      evalState (labelGears input) 1 `shouldBe` output
