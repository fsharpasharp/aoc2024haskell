{-# LANGUAGE OverloadedStrings #-}

module Day04 (solution04) where

import Solution
import Data.Maybe
import Data.Array


search :: Grid Char -> (Int, Int) -> [String]
search g (i,j) = catMaybes [horizontal, vertical, diagUp, diagDown]
  where
  limit = 3
  horizontal = sequence [g !? (i,j+x) | x <- [0..limit]]
  vertical = sequence [g !? (i+x,j) | x <- [0..limit]]
  diagUp = sequence [g !? (i+x,j+x) | x <- [0..limit]]
  diagDown = sequence [g !? (i+x,j-x) | x <- [0..limit]]


solveA :: Grid Char -> Int
solveA g = length [s | p <- range (bounds g) , s <- search g p , s == "XMAS" || s == "SAMX"]

searchMas :: Grid Char -> (Int, Int) -> Bool
searchMas g (i,j) = length allWords == 2 && all (\x -> x =="MAS" || x == "SAM") allWords
  where
    allWords = catMaybes [diagUp, diagDown]
    diagUp = sequence [g !? (i+x,j+x) | x <- [-1..1]]
    diagDown = sequence [g !? (i-x,j+x) | x <- [-1..1]]

solveB :: Grid Char -> Int
solveB g = sum [1 | p <- range (bounds g) , searchMas g p]

solution04 :: Solution (Grid Char) Int
solution04 = Solution
  { parseInput = parseGrid
  , solvePart1 = solveA
  , solvePart2 = solveB
  , files = ["data/Day04.in"]
  }
