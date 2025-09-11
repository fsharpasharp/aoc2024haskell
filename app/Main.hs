{-# LANGUAGE GADTs #-}

module Main where

import qualified Data.Map as Map

import All

-- Trick to make the list homogenous.
data AnySolution where
  AnySolution :: Show b => Solution a b -> AnySolution

solutionMap :: Map.Map Int AnySolution
solutionMap = Map.fromList
  [ (1, AnySolution solution01)
  , (2, AnySolution solution02)
  , (3, AnySolution solution03)
  , (4, AnySolution solution04)
  , (5, AnySolution solution05)
  , (6, AnySolution solution06)
  , (7, AnySolution solution07)
  ]

runDay :: Int -> IO ()
runDay n = case Map.lookup n solutionMap of
  Just (AnySolution sol) -> runSolution sol
  Nothing -> putStrLn $ "No solution for day " ++ show n

main :: IO ()
main = runDay 1
