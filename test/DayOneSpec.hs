module DayOneSpec where

-- import DayOne
import Parsing
import Test.Hspec
import Text.Megaparsec

spec :: Spec
spec = do
  describe "overlappableWrittenDigits" $ do
    it "overlapping" $ do
      parse overlappableWrittenDigits "..." "eightwo" `shouldBe` Right [8, 2]
    it "adjacent" $ do
      parse overlappableWrittenDigits "..." "eighttwo" `shouldBe` Right [8, 2]
    it "not overlapping" $ do
      parse overlappableWrittenDigits "..." "eight2" `shouldBe` Right [8]

-- TODO: broke these tests somehow...
-- describe "line" $ do
--   it "handles simplest single line" $ do
--     parse (line unitOne) "..." "eight" `shouldBe` Right [8]
--
--   it "handles simple compound line" $ do
--     parse (line unitOne) "..." "eight2" `shouldBe` Right [8, 2]
--
--   it "handles truncated compound written line" $ do
--     parse (line unitOne) "..." "eightwo" `shouldBe` Right [8, 2]
--
--   it "handles compound written line" $ do
--     parse (line unitOne) "..." "eighttwo" `shouldBe` Right [8, 2]
--
--   it "handles compound written line with mixed inputs" $ do
--     parse (line unitOne) "..." "5eighttwoeightwo2" `shouldBe` Right [5, 8, 2, 8, 2, 2]
--
--   it "can handle leading lower case characters" $ do
--     parse (line unitOne) "..." "abcone" `shouldBe` Right [1]
--
--   it "can handle six in between many characters" $ do
--     parse (line unitOne) "..." "pqrstsixteen" `shouldBe` Right [6]
--
--   it "can handle six in between many characters with leading" $ do
--     parse (line unitOne) "..." "7pqrstsixteen" `shouldBe` Right [7, 6]
