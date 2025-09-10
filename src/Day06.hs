{-# LANGUAGE OverloadedStrings #-}

module Day06 where

import Solution
import Data.Maybe
import Data.List
import qualified Data.Set as S
import Data.Array

type CharGrid = Grid Char

solv = solve "Data/Day06example.in" parseGrid solveA

data Direction = North | East | South | West deriving (Show, Enum)

data Guard = Guard (Int,Int) Direction deriving (Show)

findGuard :: CharGrid -> Guard
findGuard g = Guard guardPos North
    where guardPos = fst . fromJust . find (\(pos, val) -> val == '^') . assocs $ g


turnRight :: Direction -> Direction
turnRight West = North
turnRight d = succ d

untilOut :: CharGrid -> S.Set (Int,Int)
untilOut grid = let guard@(Guard pos dir) = findGuard grid in
    go (S.singleton pos) (findGuard grid)
  where
    go s guard =
      case next grid guard of
        Nothing   -> s
        Just g@(Guard newPos _)   -> go (S.insert newPos s) g


next :: CharGrid -> Guard -> Maybe Guard
next grid guard@(Guard cur dir) = case grid !? next of
    Nothing -> Nothing
    Just '#' -> Just (Guard cur (turnRight dir))
    _ -> Just (Guard next newDir)
    where Guard next newDir = walk guard

walk :: Guard -> Guard
walk (Guard (y,x) dir@North) = Guard (y-1,x) dir
walk (Guard (y,x) dir@East) = Guard (y,x+1) dir
walk (Guard (y,x) dir@South) = Guard (y+1,x) dir
walk (Guard (y,x) dir@West) = Guard (y,x-1) dir

s = solve "data/Day06example.in" parseGrid solveA

solutionDay06 :: Solution CharGrid Int
solutionDay06 = Solution
  { parseInput = parseGrid
  , solvePart1 = S.size . untilOut
  , solvePart2 = undefined
  }
