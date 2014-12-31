module GameOfLife
( createGrid
, cellNextState
, extractNeighborhood
, nextGrid
, Grid
, Cell
) where

import Slice

type Cell = Int
type Grid = [[Cell]]

createGrid :: Int -> [Cell] -> Grid
createGrid _ [] = []
createGrid width cells = line:(createGrid width rest)
  where (line, rest) = splitAt width cells

cellNextState :: Cell -> Int -> Cell
cellNextState cell 4 = cell
cellNextState _    3 = 1
cellNextState _    _ = 0

extractNeighborhood :: Grid -> Int -> Int -> [Cell]
extractNeighborhood generation row column
  | row == 0 = row1 ++ row2
  | row == (length generation) - 1 = row0 ++ row1
  | otherwise = row0 ++ row1 ++ row2
    where row0 = getRow $ row - 1
          row1 = getRow row
          row2 = getRow $ row + 1
          getRow r = sliceAround column $ generation !! r

nextGrid :: Grid -> Grid
nextGrid generation = [(nextRow y generation) | y <- [0..height]]
  where height = (length generation) - 1

nextRow :: Int -> Grid -> [Cell]
nextRow y generation = [(nextCell y x generation) | x <- [0..width]]
  where row = generation !! y
        width = (length row) - 1

nextCell :: Int -> Int -> Grid -> Cell
nextCell y x generation = cellNextState cell aliveNegbours
  where aliveNegbours = sum $ extractNeighborhood generation y x
        cell = (generation !! y) !! x
