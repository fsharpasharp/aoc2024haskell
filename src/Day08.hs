{-# LANGUAGE OverloadedStrings #-}

module Day08 where

import Data.Array
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace
import Solution

type CharToCoord = M.Map Char (S.Set (Int, Int))

createMap :: Grid Char -> CharToCoord
createMap grid = foldl go M.empty (assocs grid)
  where
    go m (i, e) = case e of
      '.' -> m
      _ -> M.insertWith S.union e (S.singleton i) m

data Distance = Distance
  { pos :: (Int, Int),
    dist :: (Int, Int)
  }
  deriving (Show)

antinodes :: [Distance] -> S.Set (Int, Int)
antinodes = foldl go S.empty
  where
    go s d = s `S.union` S.fromList [pos d `add` scale 2 (dist d), pos d `minus` dist d]

distances :: [(Int, Int)] -> [Distance]
distances xs = do
  (x1 : rest) <- tails xs
  x2 <- rest
  return $
    Distance
      { pos = x1,
        dist = x2 `minus` x1
      }

withinBounds ((yMin, xMin), (yMax, xMax)) = filter go
  where
    go (y, x)
      | x < xMin || y < yMin = False
      | x > xMax || y > yMax = False
      | otherwise = True

solve grid = length . withinBounds (bounds grid) . S.toList . foldl1 S.union . fmap (antinodes . distances . S.toList) $ M.elems m
  where
    m = createMap grid

sol = debug "data/Day08.in" parseGrid [solve]

solution08 :: Solution (Grid Char) Int
solution08 =
  Solution
    { parseInput = parseGrid,
      solvePart1 = solve,
      -- Extend antinodes for part 2.
      solvePart2 = solve,
      files = ["data/Day08example.in"]
    }

ss = runSolution solution08