{-# LANGUAGE OverloadedStrings #-}

module Day06 where

import Solution
import Data.List
import qualified Data.Set as S
import Data.Array
import Data.Maybe

type CharGrid = Grid Char


data Direction = North | East | South | West deriving (Show, Enum, Eq)

data Guard = Guard (Int,Int) Direction deriving (Show, Eq)

findGuard :: CharGrid -> Guard
findGuard g = Guard guardPos North
    where guardPos = fst . fromJust . find (\(_, val) -> val == '^') . assocs $ g


turnRight :: Direction -> Direction
turnRight West = North
turnRight d = succ d

untilOut :: CharGrid -> S.Set (Int,Int)
untilOut grid = let (Guard pos _) = findGuard grid in
    go (S.singleton pos) (findGuard grid)
  where
    go visited guard =
      case next grid guard of
        Nothing   -> visited
        Just g@(Guard newPos _)   -> go (S.insert newPos visited) g


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


solutionDay06 :: Solution CharGrid Int
solutionDay06 = Solution
  { parseInput = parseGrid
  , solvePart1 = S.size . untilOut
  , solvePart2 = undefined
  , files = ["data/Day06.in"]
  }

s = debug "data/Day06.in" parseGrid [S.size . untilOut]
run = runSolution solutionDay06