{-# LANGUAGE TemplateHaskell #-}

module DayTwo where

import Control.Monad (forM_)
import Data.FileEmbed (embedFile)
import Data.Functor
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as Encoding
import Parsing qualified
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline, space1)
import Text.Megaparsec.Char.Lexer (decimal)

dayTwoTest :: Text
dayTwoTest = Encoding.decodeUtf8 $(embedFile "./inputs/day2.test.txt")

gameId :: Parsing.Parser Integer
gameId = chunk "Game" *> space1 *> decimal

colon :: Parsing.Parser Char
colon = char ':'

semicolon :: Parsing.Parser Char
semicolon = char ';'

comma :: Parsing.Parser Char
comma = char ','

data Color = Blue | Red | Green
  deriving stock (Eq, Ord, Show)

color :: Parsing.Parser Color
color =
  (chunk "blue" $> Blue)
    <|> (chunk "red" $> Red)
    <|> (chunk "green" $> Green)

data Cubes = Cubes
  { cubeNumber :: Integer
  , cubeColor :: Color
  }
  deriving stock (Eq, Ord, Show)

cubes :: Parsing.Parser Cubes
cubes = Cubes <$> decimal <*> (space1 *> color)

setCubes :: Parsing.Parser [Cubes]
setCubes = sepBy1 cubes (comma >> space1)

setsCubes :: Parsing.Parser [[Cubes]]
setsCubes = sepBy1 setCubes (semicolon >> space1)

data Game = Game
  { gameIdentifier :: Integer
  , gameCubes :: [[Cubes]]
  }
  deriving stock (Show)

game :: Parsing.Parser Game
game = do
  gameIdentifier <- gameId
  colon >> space1
  gameCubes <- setsCubes
  pure Game {..}

gameBag :: Map Color Integer
gameBag =
  Map.fromList
    [ (Red, 12)
    , (Green, 13)
    , (Blue, 14)
    ]

data GameOutcome = Impossible | Possible Integer
  deriving stock (Show)

runGame :: Game -> State (Map Color Integer) GameOutcome
runGame _game = error "TODO"

solve :: Text -> [Game]
solve input =
  case parse (sepEndBy game newline) "..." input of
    Left _ -> error "unable to parse input"
    Right parsed -> parsed

main :: IO ()
main = do
  forM_ (solve dayTwoTest) print
