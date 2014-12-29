module GameOfLife
( randomCells
, createGeneration
, formatGeneration
, cellNextState
) where

import System.Random
import Data.List

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
extractNeighborhood generation row column = undefined
