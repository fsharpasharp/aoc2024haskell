{-# LANGUAGE OverloadedStrings #-}

module Day03 (solution03) where

import Solution
import Data.Maybe
import Text.Megaparsec
import Data.Functor


data Operation = Do | Dont | Result Integer


file :: Parser [Operation]
file = catMaybes <$> manyTill item eof
  where
    item :: Parser (Maybe Operation)
    item =
      (Just <$> try operation)
      <|> (anySingle $> Nothing)

operation = doParser <|> dontParser <|> mul

doParser :: Parser Operation
doParser = do
    _ <- chunk "do()"
    return Do

dontParser :: Parser Operation
dontParser = do
    _ <- chunk "don't()" 
    return Dont


mul :: Parser Operation
mul = do
    _ <- chunk "mul("
    x <- integer
    _ <- chunk ","
    y <- integer
    _ <- chunk ")"
    return $ Result (x*y)


solveA :: Operation -> Maybe Integer
solveA (Result m) = Just m
solveA _ = Nothing


solveB :: [Operation] -> Integer
solveB = snd . foldl step (True, 0)
  where
    step :: (Bool, Integer) -> Operation -> (Bool, Integer)
    step (enabled, acc) op = case op of
      Do        -> (True,  acc)
      Dont      -> (False, acc)
      Result n  -> (enabled, acc + if enabled then n else 0)


solution03 :: Solution [Operation] Integer
solution03 = Solution
  { parseInput = parseOrDie file
  , solvePart1 = sum . mapMaybe solveA
  , solvePart2 = solveB
  , files = ["data/Day03.in"]
  }
