module SizedSparseMatrix where

import Data.Foldable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | A coordinate map is an NxN dimensional array with quick lookups
data SizedSparseMatrix a = SizedSparseMatrix
  { xDimension :: Integer
  , yDimension :: Integer
  , sizedSparseMatrix :: Map (Integer, Integer) a
  }
  deriving stock (Show)

key :: Integer -> Integer -> (Integer, Integer)
key = (,)

empty :: Integer -> Integer -> SizedSparseMatrix a
empty xDimension yDimension =
  let sizedSparseMatrix = Map.empty
   in SizedSparseMatrix {..}

{- | Not concerned with the dimensions, can be used for halo search

Could introduce lookupStrict which is dimensionsally bound
-}
lookup :: Integer -> Integer -> SizedSparseMatrix a -> Maybe a
lookup x y SizedSparseMatrix {..} = Map.lookup (x, y) sizedSparseMatrix

-- | TODO: Need to test this
fromList :: [((Integer, Integer), a)] -> SizedSparseMatrix a
fromList input =
  let compareX ((x1, _), _) ((x2, _), _) = compare x1 x2
      compareY ((_, y1), _) ((_, y2), _) = compare y1 y2
      ((maxX, _), _) = maximumBy compareX input
      ((_, maxY), _) = maximumBy compareY input
   in SizedSparseMatrix
        { xDimension = maxX
        , yDimension = maxY
        , sizedSparseMatrix = Map.fromList input
        }

insert :: Integer -> Integer -> a -> SizedSparseMatrix a -> SizedSparseMatrix a
insert x y a SizedSparseMatrix {..} =
  let nextXDimension = max x xDimension
      nextYDimension = max y yDimension
   in SizedSparseMatrix
        { xDimension = nextXDimension
        , yDimension = nextYDimension
        , sizedSparseMatrix = Map.insert (x, y) a sizedSparseMatrix
        }

insertWithKey ::
  (Integer -> Integer -> a -> a -> a) ->
  Integer ->
  Integer ->
  a ->
  SizedSparseMatrix a ->
  SizedSparseMatrix a
insertWithKey f x y a SizedSparseMatrix {..} =
  let nextXDimension = max x xDimension
      nextYDimension = max y yDimension
   in SizedSparseMatrix
        { xDimension = nextXDimension
        , yDimension = nextYDimension
        , sizedSparseMatrix = Map.insertWithKey (uncurry f) (key x y) a sizedSparseMatrix
        }
