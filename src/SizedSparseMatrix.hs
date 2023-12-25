module SizedSparseMatrix where

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
