module GameOfLife
( randomCells
, createGeneration
, formatGeneration
, cellNextState
, extractNeighborhood
) where

import System.Random
import Data.List
import Slice

type Cell = Int
type Generation = [[Cell]]

randomCells :: Int -> StdGen -> [Cell]
randomCells size gen = take size $ randomRs (0, 1) gen

createGeneration :: Int -> [Cell] -> Generation
createGeneration _ [] = []
createGeneration width cells = line:(createGeneration width rest)
  where (line, rest) = splitAt width cells

formatGeneration :: Generation -> String
formatGeneration generation =
  let rows = intercalate "\n" (map (concatMap show) generation)
   in map replaceChar rows

replaceChar :: Char -> Char
replaceChar '1' = '@'
replaceChar '0' = ' '
replaceChar c   = c

cellNextState :: Cell -> [Cell] -> Cell
cellNextState cell neighborhood
  | total == 4 = cell
  | total == 3 = 1
  | otherwise = 0
    where total = sum neighborhood

extractNeighborhood :: Generation -> Int -> Int -> [Cell]
extractNeighborhood generation row column
  | row == 0 = row1 ++ row2
  | row == (length generation) - 1 = row0 ++ row1
  | otherwise = row0 ++ row1 ++ row2
    where row0 = getRow $ row - 1
          row1 = getRow row
          row2 = getRow $ row + 1
          getRow r = sliceAround column $ generation !! r
