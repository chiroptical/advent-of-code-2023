{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module DayTwo where

import Control.Monad.Trans.State
import Data.FileEmbed (embedFile)
import Data.Foldable
import Data.Functor
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as Encoding
import Parsing qualified
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (char, newline, space1)
import Text.Megaparsec.Char.Lexer (decimal)

dayTwoTest :: Text
dayTwoTest = Encoding.decodeUtf8 $(embedFile "./inputs/day2.test.txt")

dayTwo :: Text
dayTwo = Encoding.decodeUtf8 $(embedFile "./inputs/day2.txt")

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

data GameOutcome = Impossible | Possible
  deriving stock (Show)

runGame :: [Cubes] -> State (Map Color Integer) GameOutcome
runGame gameCubes = do
  forM_ gameCubes $ \Cubes {..} ->
    let alter :: Maybe Integer -> Maybe Integer
        alter = \case
          Nothing -> Just $ negate cubeNumber
          Just current -> Just $ current - cubeNumber
     in modify (Map.alter alter cubeColor)
  gameOutcome

runGames :: Game -> (Integer, GameOutcome)
runGames Game {..} =
  let foldGames :: GameOutcome -> [Cubes] -> GameOutcome
      foldGames Impossible _ = Impossible
      foldGames Possible currentCubes = evalState (runGame currentCubes) gameBag
   in (gameIdentifier, foldl' foldGames Possible gameCubes)

gameOutcome :: State (Map Color Integer) GameOutcome
gameOutcome =
  let possible :: GameOutcome -> Integer -> GameOutcome
      possible Impossible _ = Impossible
      possible Possible int = if int >= 0 then Possible else Impossible
   in Map.foldl' possible Possible <$> get

gameBag :: Map Color Integer
gameBag =
  Map.fromList
    [ (Red, 12)
    , (Green, 13)
    , (Blue, 14)
    ]

partOne :: Text -> Integer
partOne input =
  case parse (sepEndBy game newline) "..." input of
    Left _ -> error "unable to parse input"
    Right parsed ->
      let results = runGames <$> parsed
          totalPoints :: Integer -> (Integer, GameOutcome) -> Integer
          totalPoints points = \case
            (identifier, Possible) -> points + identifier
            (_, Impossible) -> points
       in foldl' totalPoints 0 results

emptyBag :: Map Color Integer
emptyBag =
  Map.fromList
    [ (Red, 0)
    , (Green, 0)
    , (Blue, 0)
    ]

alterMinimumGameSize :: [Cubes] -> State (Map Color Integer) ()
alterMinimumGameSize allCubes = forM_ allCubes $ \Cubes {..} ->
  let takeLargerCubes :: Maybe Integer -> Maybe Integer
      takeLargerCubes = \case
        Nothing -> Just cubeNumber
        Just currentCubes -> Just $ max cubeNumber currentCubes
   in modify (Map.alter takeLargerCubes cubeColor)

minimumGameSize :: Game -> State (Map Color Integer) ()
minimumGameSize Game {..} = do
  forM_ gameCubes alterMinimumGameSize

partTwo :: Text -> Integer
partTwo input =
  case parse (sepEndBy game newline) "..." input of
    Left _ -> error "unable to parse input"
    Right parsed ->
      let results = (\currentGame -> execState (minimumGameSize currentGame) emptyBag) <$> parsed
          power :: Map Color Integer -> Integer
          power = Map.foldl' (*) 1
       in foldl' (\acc minimumGameBag -> acc + power minimumGameBag) 0 results

main :: IO ()
main = do
  print ("Part 1" :: Text)
  print $ partOne dayTwoTest
  print $ partOne dayTwo

  print ("Part 2" :: Text)
  print $ partTwo dayTwoTest
  print $ partTwo dayTwo
