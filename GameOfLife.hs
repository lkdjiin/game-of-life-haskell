module GameOfLife
( createGrid
, cellNextState
, countAliveNeighbours
, nextGeneration
, Grid
, Cell(..)
, isAlive
) where

import qualified Data.Map as M
import Data.Maybe(mapMaybe)

data Cell = Alive | Dead deriving (Show, Eq, Enum)
type Pos = (Int, Int)
type Grid = M.Map Pos Cell

isAlive :: Cell -> Bool
isAlive Alive = True
isAlive _     = False

createGrid :: Int -> [Cell] -> Grid
createGrid width = M.fromList . zip cellsPos
  where cellsPos = [(x-1, y-1) | x <- [1..width], y <- [1..width]]

cellNextState :: Cell -> Int -> Cell
cellNextState cell 2 = cell
cellNextState _    3 = Alive
cellNextState _    _ = Dead

countAliveNeighbours :: Grid -> Int -> Int -> Int
countAliveNeighbours grid row column = length $ filter isAlive $ mapMaybe (flip M.lookup grid) neighboursPos
  where neighboursPos = [ (row + x, column + y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

nextGeneration :: Grid -> Grid
nextGeneration grid = M.mapWithKey (flip evolve) grid
  where evolve v = cellNextState v . uncurry (countAliveNeighbours grid)
