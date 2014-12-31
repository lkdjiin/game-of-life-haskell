module GameOfLife
( createGrid
, cellNextState
, extractNeighborhood
, nextGeneration
, Grid
, Cell
) where

import qualified Data.Map as M
import Data.Maybe(mapMaybe)

type Cell = Int
type Pos = (Int, Int)
type Grid = M.Map Pos Cell

createGrid :: Int -> [Cell] -> Grid
createGrid width = M.fromList . zip cellsPos
  where cellsPos = [(x-1, y-1) | x <- [1..width], y <- [1..width]]

cellNextState :: Cell -> Int -> Cell
cellNextState cell 2 = cell
cellNextState _    3 = 1
cellNextState _    _ = 0

extractNeighborhood :: Grid -> Int -> Int -> [Cell]
extractNeighborhood grid row column = mapMaybe (flip M.lookup grid) neighboursPos
  where neighboursPos = [ (row + x, column + y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

nextGeneration :: Grid -> Grid
nextGeneration grid = M.mapWithKey evolve grid
  where evolve (x, y) v = cellNextState v $ sum (extractNeighborhood grid x y)
