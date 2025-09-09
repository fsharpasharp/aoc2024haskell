{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Solution where

import Data.Text (Text, pack)
import Text.Megaparsec (errorBundlePretty, parse, Parsec)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

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

countTrue :: (a -> Bool) -> [a] -> Integer
countTrue f = sum . fmap (toInteger . fromEnum . f)


solve s file solve = do
  input <- pack <$> readFile s
  case parse file "" input of
    Left err -> putStrLn (errorBundlePretty err)
    Right val -> print $ solve val