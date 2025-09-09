{-# LANGUAGE OverloadedStrings #-}

module Day02 where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

integer :: Parser Integer
integer = L.signed space L.decimal

line :: Parser [Integer]
line = do
  l <- some (integer <* hspace)
  space
  return l

countTrue :: (a -> Bool) -> [a] -> Integer
countTrue f = sum . fmap (toInteger . fromEnum . f)

solveA :: [[Integer]] -> Integer
solveA = countTrue safe

safe :: [Integer] -> Bool
safe xs =
  let ds = zipWith (-) (tail xs) xs
  in  (all (> 0) ds || all (< 0) ds)
   && all ((<= 3) . abs) ds


solveB :: [[Integer]] -> Integer
solveB = countTrue safeMinusOne

safeMinusOne :: [Integer] -> Bool
safeMinusOne xs = any safe (dropOneVariants xs)

dropOneVariants :: [a] -> [[a]]
dropOneVariants ys = ys : [ take i ys ++ drop (i + 1) ys | i <- [0 .. length ys - 1] ]

solve :: IO ()
solve = do
  input <- T.pack <$> readFile "data/Day02.in"
  case parse (some line <* eof) "" input of
    Left err -> putStrLn (errorBundlePretty err)
    Right val -> print $ [solveA, solveB] <*> [val]