{-# LANGUAGE OverloadedStrings #-}

module Day04 where

import Solution
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map
import Data.Array

type Grid = Array (Int,Int) Char

fromLines :: [String] -> Grid
fromLines rows =
  let h = length rows
      w = length (head rows)
      assocs = [ ((r,c), rows !! r !! c)
               | r <- [0..h-1], c <- [0..w-1] ]
  in array ((0,0),(h-1,w-1)) assocs

(!?) :: Grid -> (Int,Int) -> Maybe Char
g !? ix = if inRange (bounds g) ix then Just (g ! ix) else Nothing

parseGrid :: Text ->  Grid
parseGrid = fromLines . lines . T.unpack


search :: Grid -> (Int, Int) -> [String]
search g (i,j) = catMaybes [horizontal, vertical, diagUp, diagDown]
  where
  limit = 3
  horizontal = sequence [g !? (i,j+x) | x <- [0..limit]]
  vertical = sequence [g !? (i+x,j) | x <- [0..limit]]
  diagUp = sequence [g !? (i+x,j+x) | x <- [0..limit]]
  diagDown = sequence [g !? (i+x,j-x) | x <- [0..limit]]


solveA :: Grid -> Int
solveA g = length [s | p <- range (bounds g) , s <- search g p , s == "XMAS" || s == "SAMX"]

searchMas :: Grid -> (Int, Int) -> Bool
searchMas g (i,j) = length words == 2 && all (\x -> x =="MAS" || x == "SAM") words
  where
    words = catMaybes [diagUp, diagDown]
    diagUp = sequence [g !? (i+x,j+x) | x <- [-1..1]]
    diagDown = sequence [g !? (i-x,j+x) | x <- [-1..1]]

solveB :: Grid -> Int
solveB g = sum [1 | p <- range (bounds g) , searchMas g p]

solutionDay04 :: Solution Grid Int
solutionDay04 = Solution
  { parseInput = parseGrid
  , solvePart1 = solveA
  , solvePart2 = solveB
  }
