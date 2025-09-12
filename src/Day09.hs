{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Day09 (solution09) where

import Data.Char (digitToInt)
import Data.Maybe
import Data.Text (unpack)
import Solution

-- This DataKind is overkill. I wanted to try it out to keep the File/FreeSpace on the type level.
-- Tbh, the only reason I did it is because the findFile returns a type an I don't want to exhaustively pattern match on Disk = File | FreeSpace
data DiskKind = IsFile | IsFree

data DiskI (k :: DiskKind) where
  File :: Int -> Int -> DiskI 'IsFile
  FreeSpace :: Int -> DiskI 'IsFree

deriving instance Show (DiskI k)

deriving instance Eq (DiskI k)

data Disk = forall k. Disk (DiskI k)

deriving instance Show Disk

file :: String -> [Disk]
file = file' 0
  where
    file' n (a : b : cs) = [Disk (File n (digitToInt a)), Disk (FreeSpace (digitToInt b))] ++ file' (n + 1) cs
    file' n [a] = [Disk (File n (digitToInt a))]
    file' _ [] = []

dropEndSpace :: [Disk] -> [Disk]
dropEndSpace = dropEndSpace' . reverse
  where
    dropEndSpace' :: [Disk] -> [Disk]
    dropEndSpace' out@(Disk (File _ _) : _) = reverse out
    dropEndSpace' (Disk (FreeSpace _) : ds) = dropEndSpace' ds
    dropEndSpace' [] = []

compact :: [Disk] -> [Disk]
compact (Disk (File a b) : xs) = Disk (File a b) : compact xs
compact everything@(Disk (FreeSpace free) : xs) = case end of
  [] -> everything
  ys -> case last ys of
    Disk (File idx occupying)
      | free == occupying -> Disk (File idx occupying) : compact (init end)
      | free < occupying -> Disk (File idx free) : compact (init end ++ [Disk (File idx (occupying - free))])
      | free > occupying -> Disk (File idx occupying) : compact (Disk (FreeSpace (free - occupying)) : init end)
    _ -> everything
  where
    end = dropEndSpace xs
compact [] = []

largestIdx :: [Disk] -> Int
largestIdx = foldl maxFile 0
  where
    maxFile :: Int -> Disk -> Int
    maxFile cur (Disk (File a _)) = max a cur
    maxFile cur _ = cur

findFile :: Int -> [Disk] -> Maybe (DiskI 'IsFile)
findFile n (Disk f@(File a _) : _) | a == n = Just f
findFile n (_ : xs) = findFile n xs
findFile _ [] = Nothing

normalize :: [Disk] -> [Disk]
normalize (Disk (FreeSpace a) : Disk (FreeSpace b) : xs) = normalize $ Disk (FreeSpace (a + b)) : xs
normalize (x : xs) = x : normalize xs
normalize [] = []

wipe :: DiskI 'IsFile -> [Disk] -> [Disk]
wipe f (Disk fil@(File _ n) : xs)
  | f == fil = Disk (FreeSpace n) : xs
  | otherwise = Disk fil : wipe f xs
wipe f (x: xs) = x : wipe f xs
wipe _ [] = []

tryInsert :: DiskI 'IsFile -> [Disk] -> [Disk]
tryInsert fil@(File _ occupied) (Disk (FreeSpace free) : xs)
  | free > occupied = Disk fil : Disk (FreeSpace (free - occupied)) : wipe fil xs
  | free == occupied = Disk fil : wipe fil xs
tryInsert f (Disk fil@(File _ _) : xs)
  | f == fil = Disk fil : xs
tryInsert f (x : xs) = x : tryInsert f xs
tryInsert _ [] = []

compactFull ds = go largest (normalize ds)
  where
    largest = largestIdx ds
    go 0 disk = disk
    go n disk = go (n - 1) (tryInsert currentFile disk)
      where
        currentFile = fromJust . findFile n $ disk

checkSum :: [Disk] -> Int
checkSum = go 0
  where
    go :: Int -> [Disk] -> Int
    go n (Disk (File a b) : xs) = (sum . fmap (* a) $ [n .. n + b - 1]) + go (n + b) xs
    go n (Disk (FreeSpace c) : xs) = go (n + c) xs
    go _ [] = 0

solution09 :: Solution [Disk] Int
solution09 =
  Solution
    { parseInput = file . unpack,
      solvePart1 = checkSum . compact,
      solvePart2 = checkSum . compactFull,
      files = ["data/Day09.in"]
    }