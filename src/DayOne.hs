{-# LANGUAGE TemplateHaskell #-}

module DayOne where

import Control.Applicative
import Control.Applicative.Combinators
import Data.FileEmbed (embedFile)
import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Text.Encoding qualified as Encoding
import Parsing qualified
import Safe
import Text.Megaparsec (parse, try)
import Text.Megaparsec.Char

dayOneTest :: Text
dayOneTest = Encoding.decodeUtf8 $(embedFile "./inputs/day1.test.txt")

dayOneTestTwo :: Text
dayOneTestTwo = Encoding.decodeUtf8 $(embedFile "./inputs/day1.test2.txt")

dayOne :: Text
dayOne = Encoding.decodeUtf8 $(embedFile "./inputs/day1.txt")

unitOne :: Parsing.Parser Integer
unitOne = try $ skipManyTill lowerChar Parsing.nonWrittenDigit

{- | 'try' is needed here because we need to backtrack in the case of
"pqrstsixteen". The parser tries to read "teen" followed by a digit, but one
isn't present.
-}
unitTwo :: Parsing.Parser Integer
unitTwo = try $ skipManyTill lowerChar Parsing.digit

line :: Parsing.Parser Integer -> Parsing.Parser [Integer]
line parser = some parser <* many lowerChar

solve :: Parsing.Parser [Integer] -> Text -> Integer
solve parser input =
  case parse (sepEndBy parser newline) "..." input of
    Left _ -> error "unable to parse input"
    Right parsed ->
      let combine acc = \case
            [] -> acc
            xs ->
              case (safeHead xs, safeTail xs) of
                (Just x, Just y) -> acc + x * 10 + y
                _ -> error "this shouldn't happen"
       in foldl' combine 0 parsed

solveOne :: Text -> Integer
solveOne = solve $ line unitOne

solveTwo :: Text -> Integer
solveTwo = solve $ line unitTwo

main :: IO ()
main = do
  print ("Part 1" :: Text)
  print $ solveOne dayOneTest
  print $ solveOne dayOne

  print ("Part 2" :: Text)
  print $ solveTwo dayOneTestTwo
  print $ solveTwo dayOne
