{-# LANGUAGE OverloadedStrings #-}

module Day08 (solution08) where

import Data.Array
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S
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
antinodes = foldl' go S.empty
  where
    go s d = s `S.union` S.fromList [pos d `add` scale 2 (dist d), pos d `minus` dist d]

fullAntinodes :: Boundary -> [Distance] -> S.Set (Int, Int)
fullAntinodes b = foldl' go S.empty
  where
    go s d = s `S.union` S.fromList (forwards ++ backwards)
      where
        forwards = takeWhile (withinBounds b) [pos d `add` scale n (dist d) | n <- [0 ..]]
        backwards = takeWhile (withinBounds b) [pos d `minus` scale n (dist d) | n <- [1 ..]]

distances :: [(Int, Int)] -> [Distance]
distances xs = do
  (x1 : rest) <- tails xs
  x2 <- rest
  return $
    Distance
      { pos = x1,
        dist = x2 `minus` x1
      }

solve anti grid = length . filter (withinBounds (bounds grid)) . S.toList . foldl1 S.union . fmap (anti . distances . S.toList) $ M.elems m
  where
    m = createMap grid

solution08 :: Solution (Grid Char) Int
solution08 =
  Solution
    { parseInput = parseGrid,
      solvePart1 = solve antinodes,
      solvePart2 = \g -> solve (fullAntinodes (bounds g)) g,
      files = ["data/Day08.in"]
    }