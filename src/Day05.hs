{-# LANGUAGE OverloadedStrings #-}

module Day05 where

import Data.Array
import qualified Data.Map
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Solution
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map.Strict as M

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
    go _ []     = True
    go forb (p:ps)
      | p `S.member` forb = False
      | otherwise =
          let forb' = forb `S.union` fromMaybe S.empty (M.lookup p beforeOf)
          in go forb' ps

solveA :: (RuleMap, [[Integer]]) -> Integer
solveA (rMap, us) = sum . fmap (\x -> x !! (length x `div` 2)) . filter (isValid rMap) $ us

solve5 = solve "data/Day05.in" file solveA

solutionDay05 :: Solution (RuleMap, [[Integer]]) Integer
solutionDay05 = Solution
  { parseInput = parseOrDie file
  , solvePart1 = solveA
  , solvePart2 = undefined
  }
