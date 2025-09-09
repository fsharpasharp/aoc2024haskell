{-# LANGUAGE OverloadedStrings #-}

module Day01 where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

integer :: Parser Integer
integer = L.signed space L.decimal

line :: Parser (Integer, Integer)
line = do
  a <- integer
  space1
  b <- integer
  space
  return (a, b)

solveA :: [(Integer, Integer)] -> Integer
solveA xs =
  let (a, b) = unzip xs
   in sum $ zipWith (\x y -> abs (x - y)) (sort a) (sort b)

occurrences :: Integer -> [Integer] -> Integer
occurrences x = toInteger . length . filter (== x)

solveB :: [(Integer, Integer)] -> Integer
solveB xs = sum [ x * occurrences x b * occurrences x a | x <- a ]
  where
    (a, b) = unzip xs


solve :: IO ()
solve = do
  input <- T.pack <$> readFile "data/Day01.in"
  case parse (some line <* eof) "" input of
    Left err -> putStrLn (errorBundlePretty err)
    Right val -> print $ [solveA, solveB] <*> [val]