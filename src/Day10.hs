module Day10  where

import Data.Array
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Solution

type ToCoord a = M.Map a (S.Set (Int, Int))

createMap :: Grid Int -> ToCoord Int
createMap grid = foldl go M.empty (assocs grid)
  where
    go m (i, e) = M.insertWith S.union e (S.singleton i) m

combinations :: Array (Int, Int) Int -> [[(Int, Int)]]
combinations g = fmap (go 0 . return) (S.toList (heights 0))
    where 
          heights x = M.findWithDefault S.empty x . createMap $ g
          go :: Int -> [(Int, Int)] -> [(Int, Int)]
          go 9 curr = curr
          go n curr = go (n+1) (concatMap (filter (`S.member` heights (n + 1)) . adjacent) curr)

parse = fmap tryDigitToInt . parseGrid
  where tryDigitToInt x | isDigit x = digitToInt x
                        | otherwise = -1


solution10 :: Solution (Grid Int) Int
solution10 =
  Solution
    { parseInput = parse,
      solvePart1 = sum . fmap (S.size . S.fromList) . combinations,
      solvePart2 = sum . fmap length . combinations,
      files = ["data/Day10.in"]
    }