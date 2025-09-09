{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Day01 where

import Solution
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


line :: Parser (Integer, Integer)
line = (,) <$> integer <* space1 <*> integer <* space

occurrences :: Integer -> [Integer] -> Integer
occurrences x = toInteger . length . filter (== x)

solutionDay01 :: Solution [(Integer, Integer)] Integer
solutionDay01 = Solution
  { parseInput = parseOrDie $ some line <* eof
  , solvePart1 = \xs -> let (a, b) = unzip xs in sum $ zipWith (\x y -> abs (x - y)) (sort a) (sort b)
  , solvePart2 = \xs -> let (a,b) = unzip xs in sum [ x * occurrences x b * occurrences x a | x <- a ]
  }
