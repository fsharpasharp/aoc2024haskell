{-# LANGUAGE MultiParamTypeClasses #-}
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
      as = [ ((r,c), rows !! r !! c)
               | r <- [0..h-1], c <- [0..w-1] ]
  in array ((0,0),(h-1,w-1)) as

(!?) :: Grid Char -> (Int,Int) -> Maybe Char
g !? ix = if inRange (bounds g) ix then Just (g ! ix) else Nothing

parseGrid :: Text ->  Grid Char
parseGrid = fromLines . lines . unpack

type Parser = Parsec Void Text


data Solution a b = Solution
  { parseInput  :: Text -> a
  , solvePart1  :: a -> b
  , solvePart2  :: a -> b
  , files :: [FilePath]
  }

parseOrDie :: Parser a -> Text -> a
parseOrDie p t =
  case parse p "" t of
    Left e  -> error (errorBundlePretty e)
    Right x -> x

runSolution :: Show b => Solution a b -> IO ()
runSolution sol = do
  inputs <- mapM (fmap pack . readFile) (files sol)
  let xs = map (parseInput sol) inputs
      part1Results = map (solvePart1 sol) xs
      part2Results = map (solvePart2 sol) xs
  putStrLn "Part 1:"
  mapM_ print (files sol `zip` part1Results )
  putStrLn "Part 2:"
  mapM_ print (files sol `zip` part2Results)


integer :: Parser Integer
integer = L.signed space L.decimal

debug :: Show b => FilePath -> (Text -> a) -> [a -> b] -> IO ()
debug filePath parser solvers = do
  input <- pack <$> readFile filePath
  print $ solvers <*> [parser input]