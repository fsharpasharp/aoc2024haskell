{-# LANGUAGE OverloadedStrings #-}

module Day07 where

import Solution
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad

data Result = Result Integer [Integer] deriving (Show)

file :: Parser [Result]
file = some $ (do
    res <- integer
    _ <- chunk ": "
    rest <- some (integer <* hspace)
    return $ Result res rest) <* space


-- Optimization here to stop computing if we overshoot.
values _ []     = []
values ops (x:xs) = foldM step x xs
  where
    step acc y = [ op acc y | op <- ops ]


solve ops = sum . fmap solve'
    where solve' (Result ans comp) = if ans `elem` values ops comp then ans else 0

solution07 :: Solution [Result] Integer
solution07 = Solution
  { parseInput = parseOrDie $ file <* eof
  , solvePart1 = solve [(+), (*)]
  , solvePart2 = solve [(+), (*), \a b -> read (show a ++ show b)]
  , files = ["data/Day07.in"]
  }