{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import Solution
import Text.Megaparsec
import Text.Megaparsec.Char

data Result = Result Integer [Integer] deriving (Show)

file :: Parser [Result]
file = some $ do
    res <- integer
    _ <- chunk ": "
    rest <- some (integer <* space)
    return $ Result res rest

s = debug "data/Day07example.in" (parseOrDie file) [id]

solution07 = undefined
solution07 :: Solution [Result] Integer
solution07 = Solution
  { parseInput = file
  , solvePart1 = undefined
  , solvePart2 = undefined
  , files = ["data/Day07example.in"]
  }