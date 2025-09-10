{-# LANGUAGE GADTs #-}

module Main where

import qualified Data.Map as Map

import All

-- Trick to make the list homogenous.
data AnySolution where
  AnySolution :: Show b => Solution a b -> AnySolution

as :: Solution a b -> AnySolution
as = AnySolution

solutionMap :: Map.Map Int AnySolution
solutionMap = Map.fromList
  [ (1, as solution01)
  , (2, as solution02)
  , (3, as solution03)
  , (4, as solution04)
  , (5, as solution05)
  , (6, as solution06)
  ]

runDay :: Int -> IO ()
runDay n = case Map.lookup n solutionMap of
  Just (AnySolution sol) -> runSolution sol
  Nothing -> putStrLn $ "No solution for day " ++ show n

main :: IO ()
main = runDay 1
