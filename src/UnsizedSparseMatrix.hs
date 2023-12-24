module UnsizedSparseMatrix where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | A coordinate map is an NxN dimensional array with quick lookups
newtype UnsizedSparseMatrix a = UnsizedSparseMatrix
  { unUnsizedSparseMatrix :: Map (Integer, Integer) a
  }
  deriving stock (Show)

key :: Integer -> Integer -> (Integer, Integer)
key = (,)

empty :: UnsizedSparseMatrix a
empty = UnsizedSparseMatrix Map.empty

lookup :: Integer -> Integer -> UnsizedSparseMatrix a -> Maybe a
lookup x y UnsizedSparseMatrix {..} = Map.lookup (x, y) unUnsizedSparseMatrix

insert :: Integer -> Integer -> a -> UnsizedSparseMatrix a -> UnsizedSparseMatrix a
insert x y a UnsizedSparseMatrix {..} =
  UnsizedSparseMatrix $ Map.insert (x, y) a unUnsizedSparseMatrix

insertWithKey ::
  (Integer -> Integer -> a -> a -> a) ->
  Integer ->
  Integer ->
  a ->
  UnsizedSparseMatrix a ->
  UnsizedSparseMatrix a
insertWithKey f x y a UnsizedSparseMatrix {..} =
  UnsizedSparseMatrix $ Map.insertWithKey (uncurry f) (key x y) a unUnsizedSparseMatrix
