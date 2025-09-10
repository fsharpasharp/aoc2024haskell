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
import Data.Bifunctor


line :: Parser (Integer, Integer)
line = (,) <$> integer <* space1 <*> integer <* space

occurrences :: Integer -> [Integer] -> Integer
occurrences x = toInteger . length . filter (== x)

yay (a,b) = zipWith (\x y -> abs (x - y)) (sort a) (sort b)

solved = solve "data/Day01.in" (parseOrDie (fmap unzip $ some line <* eof)) yay

solutionDay01 :: Solution ([Integer], [Integer]) Integer
solutionDay01 = Solution
  { parseInput = parseOrDie $ fmap unzip (some line <* eof)
  , solvePart1 = sum . fmap abs . uncurry (zipWith (-)) . bimap sort sort
  , solvePart2 = \(a,b) -> sum [ x * occurrences x b * occurrences x a | x <- a ]
  }
