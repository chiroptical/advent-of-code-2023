{-# LANGUAGE ViewPatterns #-}

module Safe where

safeHead :: [a] -> Maybe a
safeHead = \case
  [] -> Nothing
  (x : _) -> Just x

safeTail :: [a] -> Maybe a
safeTail = \case
  [] -> Nothing
  (last -> x) -> Just x
