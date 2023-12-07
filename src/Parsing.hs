module Parsing where

import Data.Functor
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec

type Parser = Parsec Void Text

chunkUnsnoc :: Text -> Parser Text
chunkUnsnoc input =
  case Text.unsnoc input of
    Nothing -> fail "chunkUnsnoc: empty input"
    Just (begin, end) -> chunk begin <* lookAhead (single end)

writtenDigits :: Parser [Integer]
writtenDigits = many writtenDigit

writtenDigit :: Parser Integer
writtenDigit =
  (chunkUnsnoc "one" $> 1)
    <|> (chunkUnsnoc "two" $> 2)
    <|> (chunkUnsnoc "three" $> 3)
    <|> (chunkUnsnoc "four" $> 4)
    <|> (chunkUnsnoc "five" $> 5)
    <|> (chunkUnsnoc "six" $> 6)
    <|> (chunkUnsnoc "seven" $> 7)
    <|> (chunkUnsnoc "eight" $> 8)
    <|> (chunkUnsnoc "nine" $> 9)

digit :: Parser Integer
digit =
  (chunk "1" $> 1)
    <|> (chunk "2" $> 2)
    <|> (chunk "3" $> 3)
    <|> (chunk "4" $> 4)
    <|> (chunk "5" $> 5)
    <|> (chunk "6" $> 6)
    <|> (chunk "7" $> 7)
    <|> (chunk "8" $> 8)
    <|> (chunk "9" $> 9)
