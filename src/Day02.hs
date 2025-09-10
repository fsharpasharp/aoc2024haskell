module Day02 where

import Solution
import Text.Megaparsec
import Text.Megaparsec.Char

line :: Parser [Integer]
line = do
  l <- some (integer <* hspace)
  space
  return l

safe :: [Integer] -> Bool
safe xs =
  let differences = zipWith (-) (tail xs) xs
      striclyIncreasing = all (>0) differences
      striclyDecreasing = all (<0) differences
      notLargerThan3 = all ((<= 3) . abs) differences
  in  (striclyIncreasing || striclyDecreasing) && notLargerThan3

safeMinusOne :: [Integer] -> Bool
safeMinusOne xs = any safe (dropOneVariants xs)
  where dropOneVariants :: [a] -> [[a]]
        dropOneVariants ys = ys : [ take i ys ++ drop (i + 1) ys | i <- [0 .. length ys - 1] ]

solutionDay02 :: Solution [[Integer]] Int
solutionDay02 = Solution
  { parseInput = parseOrDie $ some line <* eof
  , solvePart1 = length . filter safe
  , solvePart2 = length . filter safeMinusOne
  , files = ["data/Day02.in"]
  }
