{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module DayThree where

import Control.Monad.Trans.State
import Data.FileEmbed (embedFile)
import Data.Foldable
import Data.Text (Text)
import Data.Text.Encoding qualified as Encoding
import Parsing
import SizedSparseMatrix (SizedSparseMatrix)
import SizedSparseMatrix qualified as Matrix
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, newline)

dayThreeTest :: Text
dayThreeTest = Encoding.decodeUtf8 $(embedFile "./inputs/day3.test.txt")

dayThree :: Text
dayThree = Encoding.decodeUtf8 $(embedFile "./inputs/day3.txt")

data NumberOrSymbol
  = Number Integer
  | Symbol Char
  | Period
  deriving stock (Show, Eq)

numberOrSymbol :: Parser NumberOrSymbol
numberOrSymbol =
  let number = Number <$> nonWrittenDigit <?> "nonWrittenDigit"
      symbol = Symbol <$> noneOf ("\n." :: String) <?> "not period"
      period = Period <$ char '.'
   in number <|> symbol <|> period

line :: Parser [NumberOrSymbol]
line = Text.Megaparsec.some numberOrSymbol

foldInternal ::
  SizedSparseMatrix NumberOrSymbol ->
  (Integer, [(Integer, NumberOrSymbol)]) ->
  SizedSparseMatrix NumberOrSymbol
foldInternal matrix (x, xs) =
  let folder ::
        SizedSparseMatrix NumberOrSymbol ->
        (Integer, NumberOrSymbol) ->
        SizedSparseMatrix NumberOrSymbol
      folder acc (y, ns) = Matrix.insert x y ns acc
   in foldl' folder matrix xs

fromZipped ::
  Integer ->
  [(Integer, [(Integer, NumberOrSymbol)])] ->
  SizedSparseMatrix NumberOrSymbol
fromZipped dimension = foldl' foldInternal (Matrix.empty dimension dimension)

-- | Make an assumption that '[[NumberOrSymbol]]' is square...
toUnsizedSparseMatrix ::
  [[NumberOrSymbol]] ->
  SizedSparseMatrix NumberOrSymbol
toUnsizedSparseMatrix matrix =
  let dimension = length matrix
      sizedSparseMatrix =
        fromZipped (toInteger dimension) $
          fmap (zip [0 ..]) <$> zip [0 ..] matrix
   in sizedSparseMatrix

searchOne :: (a -> Bool) -> Integer -> Integer -> SizedSparseMatrix a -> Bool
searchOne f x y mat =
  let indices = (,) <$> [x - 1 .. x + 1] <*> [y - 1 .. y + 1]
      check = maybe False f
      folder acc (xIndex, yIndex) =
        if xIndex == x && yIndex == y
          then acc
          else acc || check (Matrix.lookup xIndex yIndex mat)
   in foldl' folder False indices

isSymbol :: NumberOrSymbol -> Bool
isSymbol = \case
  Symbol _ -> True
  _ -> False

data WithSymbol = WithoutSymbol | WithSymbol
  deriving stock (Show, Eq)

instance Semigroup WithSymbol where
  WithSymbol <> _ = WithSymbol
  _ <> WithSymbol = WithSymbol
  _ <> _ = WithoutSymbol

data CurrentNumber = CurrentNumber
  { currentNumber :: [Integer]
  , currentNumberHasSymbol :: WithSymbol
  }

emptyCurrentNumber :: CurrentNumber
emptyCurrentNumber = CurrentNumber {currentNumber = [], currentNumberHasSymbol = WithoutSymbol}

-- | Searh a row for numbers,
searchRow ::
  Integer ->
  Integer ->
  SizedSparseMatrix NumberOrSymbol ->
  State (CurrentNumber, [Integer]) [Integer]
searchRow dimension row matrix = do
  forM_ [0 .. dimension - 1] $ \column -> do
    case Matrix.lookup row column matrix of
      Nothing -> error "this shouldn't happen"
      -- Found a number, push it onto currentNumber and search for symbol
      Just (Number x) -> do
        (CurrentNumber {..}, numbers) <- get
        let nextWithSymbol =
              if currentNumberHasSymbol == WithSymbol || searchOne isSymbol row column matrix
                then WithSymbol
                else WithoutSymbol
        if column == dimension - 1
          then case nextWithSymbol of
            WithoutSymbol -> put (emptyCurrentNumber, numbers)
            WithSymbol -> put (emptyCurrentNumber, numbers <^> toNumber (currentNumber <^> x))
          else
            put
              ( CurrentNumber
                  { currentNumber = currentNumber <^> x
                  , currentNumberHasSymbol = nextWithSymbol
                  }
              , numbers
              )
      -- Hit something that isn't a number, add the number to numbers if we encountered a symbol
      Just _ -> do
        (CurrentNumber {..}, numbers) <- get
        case currentNumberHasSymbol of
          WithoutSymbol -> put (emptyCurrentNumber, numbers)
          WithSymbol -> put (emptyCurrentNumber, numbers <^> toNumber currentNumber)
  snd <$> get

-- | This is 'snoc'
(<^>) :: [a] -> a -> [a]
xs <^> x = xs <> [x]

toNumber :: [Integer] -> Integer
toNumber xs =
  let withPrefactor = zip [0 ..] $ reverse xs
      multiplyPrefactorByTen :: Integer -> (Integer, Integer) -> Integer
      multiplyPrefactorByTen acc (prefactor, x) = acc + (10 ^ prefactor) * x
   in foldl' multiplyPrefactorByTen 0 withPrefactor

partOne :: Text -> Integer
partOne input =
  case parse (sepEndBy1 line newline) "..." input of
    Left _ -> error "unable to parse input"
    Right parsed ->
      let sizedSparseMatrix = toUnsizedSparseMatrix parsed
          runSearch :: [Integer] -> Integer -> [Integer]
          runSearch acc row =
            acc <> evalState (searchRow sizedSparseMatrix.yDimension row sizedSparseMatrix) (emptyCurrentNumber, [])
          numbers = foldl' runSearch [] [0 .. sizedSparseMatrix.xDimension - 1]
       in foldl' (+) 0 numbers

{- | To solve this, we need to extend 'NumberOrSymbol' with 'Gear Integer'
where each 'Gear' is the symbol '*' with a monotonically increasing number.

Instead of collecting all the numbers, we need to collect numbers next to
'Gear's and retain which 'Gear' they were connected to. Then, we collect
numbers which are attached to exactly 2 gears and multiply the numbers
together. Finally, we sum all of the gear ratios
-}
partTwo :: Text -> [[NumberOrSymbol]]
partTwo input =
  case parse (sepEndBy1 line newline) "..." input of
    Left _ -> error "unable to parse input"
    Right parsed -> parsed

main :: IO ()
main = do
  print ("Part 1" :: Text)
  print $ partOne dayThreeTest
  print $ partOne dayThree

  print ("Part 2" :: Text)
