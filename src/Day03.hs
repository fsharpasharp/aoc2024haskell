{-# LANGUAGE OverloadedStrings #-}

module Day03 where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

integer :: Parser Integer
integer = L.signed space L.decimal

data Operation = Do | Dont | Result Integer


file :: Parser [Operation]
file = catMaybes <$> manyTill item eof
  where
    item :: Parser (Maybe Operation)
    item =
      (Just <$> try operation)
      <|> (anySingle *> pure Nothing)

operation = doParser <|> dontParser <|> mul

doParser :: Parser Operation
doParser = do
    chunk "do()"
    return Do

dontParser :: Parser Operation
dontParser = do
    chunk "don't()" 
    return Dont


mul :: Parser Operation
mul = do
    _ <- chunk "mul("
    x <- integer
    _ <- chunk ","
    y <- integer
    _ <- chunk ")"
    return $ Result (x*y)

solveA :: [Operation] -> Integer
solveA = sum . mapMaybe solveA'

solveA' :: Operation -> Maybe Integer
solveA' (Result m) = Just m
solveA' _ = Nothing


solveB :: [Operation] -> Integer
solveB = snd . foldl step (True, 0)
  where
    step :: (Bool, Integer) -> Operation -> (Bool, Integer)
    step (enabled, acc) op = case op of
      Do        -> (True,  acc)
      Dont      -> (False, acc)
      Result n  -> (enabled, acc + if enabled then n else 0)

solve :: IO ()
solve = do
  input <- T.pack <$> readFile "data/Day03.in"
  case parse file "" input of
    Left err -> putStrLn (errorBundlePretty err)
    Right val -> print $ [solveA, solveB] <*> [val]