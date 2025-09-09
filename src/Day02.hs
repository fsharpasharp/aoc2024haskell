module Day02 where

import Solution
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

line :: Parser [Integer]
line = do
  l <- some (integer <* hspace)
  space
  return l

safe :: [Integer] -> Bool
safe xs =
  let ds = zipWith (-) (tail xs) xs
  in  (all (> 0) ds || all (< 0) ds)
   && all ((<= 3) . abs) ds

safeMinusOne :: [Integer] -> Bool
safeMinusOne xs = any safe (dropOneVariants xs)

dropOneVariants :: [a] -> [[a]]
dropOneVariants ys = ys : [ take i ys ++ drop (i + 1) ys | i <- [0 .. length ys - 1] ]

solutionDay02 :: Solution [[Integer]] Integer
solutionDay02 = Solution
  { parseInput = parseOrDie $ some line <* eof
  , solvePart1 = countTrue safe
  , solvePart2 = countTrue safeMinusOne
  }
