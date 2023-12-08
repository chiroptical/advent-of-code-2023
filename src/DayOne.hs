{-# LANGUAGE TemplateHaskell #-}

module DayOne where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Monad (join)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Text.Encoding qualified as Encoding
import Debug.Trace (trace)
import Parsing qualified
import Safe
import Text.Megaparsec (anySingle, parse)
import Text.Megaparsec.Char

dayOneTest :: Text
dayOneTest = Encoding.decodeUtf8 $(embedFile "./inputs/day1.test.txt")

dayOneTestTwo :: Text
dayOneTestTwo = Encoding.decodeUtf8 $(embedFile "./inputs/day1.test2.txt")

dayOne :: Text
dayOne = Encoding.decodeUtf8 $(embedFile "./inputs/day1.txt")

block :: Parsing.Parser Integer
block = skipManyTill lowerChar Parsing.digit

line :: Parsing.Parser [Integer]
line = some block <* many lowerChar

solveTwo :: Text -> Integer
solveTwo input =
  case parse (sepEndBy line newline) "..." input of
    Left _ -> error "unable to parse input"
    Right parsed ->
      let combine acc = \case
            [] -> acc
            xs ->
              case (safeHead xs, safeTail xs) of
                (Just x, Just y) -> acc + x * 10 + y
                _ -> error "this shouldn't happen"
       in foldl' combine 0 (trace (show parsed) parsed)

main :: IO ()
main = do
  print $ solveTwo dayOneTest
  print $ solveTwo dayOneTestTwo

-- print $ solveTwo dayOne
