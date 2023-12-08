module Parsing where

import Control.Monad (join)
import Data.Functor
import Data.List (singleton)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Debug.Trace (trace)
import Text.Megaparsec

type Parser = Parsec Void Text

invalidNumberChars :: Parser Char
invalidNumberChars = noneOf ("otfsen123456789" :: String)

-- eightwo - overlapping, don't consume end character ('t')
-- eighttwo - adjacent, consume end character
-- eight2 -- no overlap, consume end character
data OverlapKind
  = Overlapping
  | Adjacent
  | NoOverlap

writtenNumber :: Text -> Integer -> Parser Integer
writtenNumber input output =
  case Text.unsnoc input of
    Nothing -> fail "empty input"
    Just (begin, end) -> do
      chunk begin
      let endChar = chunk (Text.singleton end)
          overlapping = Overlapping <$ writtenDigit
          adjacent = Adjacent <$ (endChar <* writtenDigit)
          noOverlap = NoOverlap <$ endChar
      overlapKind <- lookAhead $ try overlapping <|> try adjacent <|> try noOverlap
      case overlapKind of
        Overlapping ->
          pure output
        Adjacent -> do
          endChar
          pure output
        NoOverlap -> do
          endChar
          pure output

writtenDigit :: Parser Integer
writtenDigit =
  (chunk "one" $> 1)
    <|> (chunk "two" $> 2)
    <|> (chunk "three" $> 3)
    <|> (chunk "four" $> 4)
    <|> (chunk "five" $> 5)
    <|> (chunk "six" $> 6)
    <|> (chunk "seven" $> 7)
    <|> (chunk "eight" $> 8)
    <|> (chunk "nine" $> 9)

overlappableWrittenDigit :: Parser Integer
overlappableWrittenDigit =
  writtenNumber "one" 1
    <|> writtenNumber "two" 2
    <|> writtenNumber "three" 3
    <|> writtenNumber "four" 4
    <|> writtenNumber "five" 5
    <|> writtenNumber "six" 6
    <|> writtenNumber "seven" 7
    <|> writtenNumber "eight" 8
    <|> writtenNumber "nine" 9

overlappableWrittenDigits :: Parser [Integer]
overlappableWrittenDigits = many overlappableWrittenDigit

nonWrittenDigit :: Parser Integer
nonWrittenDigit =
  (chunk "1" $> 1)
    <|> (chunk "2" $> 2)
    <|> (chunk "3" $> 3)
    <|> (chunk "4" $> 4)
    <|> (chunk "5" $> 5)
    <|> (chunk "6" $> 6)
    <|> (chunk "7" $> 7)
    <|> (chunk "8" $> 8)
    <|> (chunk "9" $> 9)

digit :: Parser Integer
digit = overlappableWrittenDigit <|> nonWrittenDigit

digits :: Parser [Integer]
digits = many digit
