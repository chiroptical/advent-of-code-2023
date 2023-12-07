{-# LANGUAGE TemplateHaskell #-}

module DayOne where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Monad (join)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text.Encoding qualified as Encoding
import Parsing qualified
import Text.Megaparsec (try)
import Text.Megaparsec.Char

dayOneTest :: Text
dayOneTest = Encoding.decodeUtf8 $(embedFile "./inputs/day1.test.txt")

digit :: Parsing.Parser Integer
digit = Parsing.writtenDigit <|> Parsing.digit

line :: Parsing.Parser [Integer]
line = join <$> sepEndBy (many digit) (some letterChar)

main :: IO ()
main = print dayOneTest
