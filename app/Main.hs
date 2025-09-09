{-# LANGUAGE GADTs #-}

module Main where

import qualified Data.Text as T
import qualified Data.Map as Map

import Solution
import Day01
import Day02
import Day03
import Day04

data AnySolution where
  AnySolution :: Show b => [FilePath] -> Solution a b -> AnySolution

solutionMap :: Map.Map Int AnySolution
solutionMap = Map.fromList
  [ (1, AnySolution ["data/Day01.in"] solutionDay01)
  , (2, AnySolution ["data/Day02.in"] solutionDay02)
  , (3, AnySolution ["data/Day03.in"] solutionDay03)
  , (4, AnySolution ["data/Day04example.in", "data/Day04.in"] solutionDay04)
  ]

runDay :: Int -> IO ()
runDay n = case Map.lookup n solutionMap of
  Just (AnySolution files sol) -> do
    inputs <- mapM (fmap T.pack . readFile) files
    runSolution sol inputs
  Nothing -> putStrLn $ "No solution for day " ++ show n

main :: IO ()
main = do
  runDay 1
