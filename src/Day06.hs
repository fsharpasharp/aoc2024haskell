{-# LANGUAGE OverloadedStrings #-}

module Day06 (solution06) where

import Solution
import Data.List
import qualified Data.Set as S
import Data.Array
import Data.Maybe

type CharGrid = Grid Char
data Direction = North | East | South | West deriving (Show, Enum, Eq, Ord)
data Guard = Guard (Int,Int) Direction deriving (Show, Eq, Ord)

findGuard :: CharGrid -> Guard
findGuard g = Guard guardPos North
    where guardPos = fst . fromJust . find (\(_, val) -> val == '^') . assocs $ g

turnRight :: Direction -> Direction
turnRight West = North
turnRight d = succ d

next :: CharGrid -> Guard -> Maybe Guard
next grid guard@(Guard cur dir) = case grid !? nextPos of
    Nothing -> Nothing
    Just '#' -> Just (Guard cur (turnRight dir))
    _ -> Just (Guard nextPos newDir)
    where Guard nextPos newDir = walk guard

walk :: Guard -> Guard
walk (Guard (y,x) dir@North) = Guard (y-1,x) dir
walk (Guard (y,x) dir@East) = Guard (y,x+1) dir
walk (Guard (y,x) dir@South) = Guard (y+1,x) dir
walk (Guard (y,x) dir@West) = Guard (y,x-1) dir

untilOut :: CharGrid -> S.Set (Int,Int)
untilOut grid = let guard@(Guard pos _) = findGuard grid in
    go (S.singleton pos) guard
  where
    go visited guard =
      case next grid guard of
        Nothing   -> visited
        Just g@(Guard newPos _)   -> go (S.insert newPos visited) g

countLoops :: CharGrid -> S.Set (Int,Int) -> Int
countLoops grid = length . filter countLoops' . S.toList . S.delete pos
  where guard@(Guard pos _) = findGuard grid
        countLoops' c = isLoop (grid // [(c, '#')]) guard

isLoop :: CharGrid -> Guard -> Bool
isLoop grid = go S.empty
  where 
    go visited guard = guard `S.member` visited ||
        case next grid guard of 
          Nothing -> False
          Just g -> go (S.insert guard visited) g

-- Slow! Optimize later.
solveB grid = countLoops grid candidateSet
  where candidateSet = untilOut grid

solution06 :: Solution CharGrid Int
solution06 = Solution
  { parseInput = parseGrid
  , solvePart1 = S.size . untilOut
  , solvePart2 = solveB
  , files = ["data/Day06example.in"]
  }