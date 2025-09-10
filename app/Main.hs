{-# LANGUAGE GADTs #-}

module Main where

import qualified Data.Map as Map

import Solution
import Day01
import Day02
import Day03
import Day04
import Day05

-- Trick to make the list homogenous.
data AnySolution where
  AnySolution :: Show b => Solution a b -> AnySolution

solutionMap :: Map.Map Int AnySolution
solutionMap = Map.fromList
  [ (1, AnySolution solutionDay01)
  , (2, AnySolution solutionDay02)
  , (3, AnySolution solutionDay03)
  , (4, AnySolution solutionDay04)
  , (5, AnySolution solutionDay05)
  ]

runDay :: Int -> IO ()
runDay n = case Map.lookup n solutionMap of
  Just (AnySolution sol) -> runSolution sol
  Nothing -> putStrLn $ "No solution for day " ++ show n

main :: IO ()
main = runDay 1
