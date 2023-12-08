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
import Text.Megaparsec (anySingle, try)
import Text.Megaparsec.Char

dayOneTest :: Text
dayOneTest = Encoding.decodeUtf8 $(embedFile "./inputs/day1.test.txt")

line :: Parsing.Parser [Integer]
line = join <$> sepEndBy Parsing.digits (some lowerChar)

main :: IO ()
main = print dayOneTest
