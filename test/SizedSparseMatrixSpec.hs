module SizedSparseMatrixSpec where

import Data.Map.Strict qualified as Map
import SizedSparseMatrix qualified as Matrix
import Test.Hspec

spec :: Spec
spec = do
  describe "fromList" $ do
    it "works as Map's fromList for empty input with added dimensions" $ do
      let output =
            Matrix.SizedSparseMatrix
              { xDimension = 0
              , yDimension = 0
              , sizedSparseMatrix = Map.empty :: Map.Map (Integer, Integer) Integer
              }
      Matrix.fromList [] `shouldBe` output
    it "works as Map's fromList with added dimensions" $ do
      let input :: [((Integer, Integer), Integer)]
          input = [((1, 1), 0)]
          output =
            Matrix.SizedSparseMatrix
              { xDimension = 1
              , yDimension = 1
              , sizedSparseMatrix = Map.fromList input
              }
      Matrix.fromList input `shouldBe` output
