{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Solution where

import Data.Text (Text, pack, unpack)
import Text.Megaparsec (errorBundlePretty, parse, Parsec)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Array

type Grid a = Array (Int,Int) a

fromLines :: [String] -> Grid Char
fromLines rows =
  let h = length rows
      w = length (head rows)
      assocs = [ ((r,c), rows !! r !! c)
               | r <- [0..h-1], c <- [0..w-1] ]
  in array ((0,0),(h-1,w-1)) assocs

(!?) :: Grid Char -> (Int,Int) -> Maybe Char
g !? ix = if inRange (bounds g) ix then Just (g ! ix) else Nothing

parseGrid :: Text ->  Grid Char
parseGrid = fromLines . lines . unpack

type Parser = Parsec Void Text


data Solution a b = Solution
  { parseInput  :: Text -> a
  , solvePart1  :: a -> b
  , solvePart2  :: a -> b
  }

parseOrDie :: Parser a -> Text -> a
parseOrDie p t =
  case parse p "" t of
    Left e  -> error (errorBundlePretty e)
    Right x -> x

runSolution :: Show b => Solution a b -> [Text] -> IO ()
runSolution sol ts = do
  let xs = map (parseInput sol) ts
      part1Results = map (solvePart1 sol) xs
      part2Results = map (solvePart2 sol) xs
  putStrLn "Part 1:"
  mapM_ print part1Results
  putStrLn "Part 2:"
  mapM_ print part2Results

integer :: Parser Integer
integer = L.signed space L.decimal

solve filePath parser solve = do
  input <- pack <$> readFile filePath
  print $ solve . parser $ input