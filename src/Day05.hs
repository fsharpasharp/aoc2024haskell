{-# LANGUAGE OverloadedStrings #-}

module Day05 where

import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Solution
import Text.Megaparsec
  ( MonadParsec (eof),
    chunk,
    sepBy1,
    sepEndBy,
    some,
  )
import Text.Megaparsec.Char (eol, newline, space)
import qualified Text.Megaparsec.Char.Lexer as L

data Rule = Rule Integer Integer -- Before/After
  deriving (Show)

type RuleMap = M.Map Integer (S.Set Integer)

rulesMap :: [Rule] -> RuleMap
rulesMap = foldl step M.empty
  where
    step m (Rule before after) = M.insertWith S.union after (S.singleton before) m

rules :: Parser [Rule]
rules =
  some
    ( do
        a <- integer
        chunk "|"
        b <- integer
        newline
        return $ Rule a b
    )

updates :: Parser [[Integer]]
updates = (integer `sepBy1` chunk ",") `sepEndBy` eol

file = do
  rules <- rules
  space
  updates <- updates
  eof
  return (rulesMap rules, updates)

isValid :: RuleMap -> [Integer] -> Bool
isValid beforeOf = go S.empty
  where
    go _ [] = True
    go forb (p : ps)
      | p `S.member` forb = False
      | otherwise =
          let forb' = forb `S.union` M.findWithDefault S.empty p beforeOf
           in go forb' ps

solveA :: (RuleMap, [[Integer]]) -> Integer
solveA (rMap, us) = sum . fmap (\x -> x !! (length x `div` 2)) . filter (isValid rMap) $ us

beforeSets beforeOf update =
  let relevant = S.fromList update
   in fmap (\x -> S.size (relevant `S.intersection` M.findWithDefault S.empty x beforeOf)) update

solveB :: (RuleMap, [[Integer]]) -> Integer
solveB (rMap, us) =
  let invalids = filter (not . isValid rMap) us
      sorted = fmap (\x -> sort $ beforeSets rMap x `zip` x) invalids
   in sum . fmap (snd . \x -> x !! (length x `div` 2)) $ sorted

solutionDay05 :: Solution (RuleMap, [[Integer]]) Integer
solutionDay05 =
  Solution
    { parseInput = parseOrDie file,
      solvePart1 = solveA,
      solvePart2 = solveB
    }
