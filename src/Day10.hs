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

solve :: Array (Int, Int) Int -> Int
solve g = sum . fmap (go 0 . S.singleton) . S.toList . heights $ 0
    where heights x = M.findWithDefault S.empty x (createMap g)
          go :: Int -> S.Set (Int, Int) -> Int
          go 9 curr = S.size curr
          go n curr = go (n+1) (allAdjacent `S.intersection` heights (n+1))
            where allAdjacent = foldl S.union S.empty (fmap (S.fromList . adjacent) (S.toList curr))


s = debug "data/Day10.in" parse [solve]

parse = fmap tryDigitToInt . parseGrid
  where tryDigitToInt x | isDigit x = digitToInt x
                        | otherwise = -1


solution10 :: Solution (Grid Int) Int
solution10 =
  Solution
    { parseInput = parse,
      solvePart1 = solve,
      solvePart2 = solve,
      files = ["data/Day10example.in"]
    }