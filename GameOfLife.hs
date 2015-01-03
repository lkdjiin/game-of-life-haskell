module GameOfLife
( createGrid
, cellNextState
, countAliveNeighbours
, nextGeneration
, Grid
, Cell(..)
, Pos(..)
, isAlive
) where

import qualified Data.Map as M
import Data.Maybe(mapMaybe)

data Cell = Alive | Dead deriving (Show, Eq, Enum)
data Pos = Pos { getX :: Int, getY :: Int } deriving (Show, Eq)
type Grid = M.Map Pos Cell

isAlive :: Cell -> Bool
isAlive = (== Alive)

instance Ord Pos where
    compare (Pos x y) (Pos x' y') = case compare x x' of
                                      EQ -> compare y y'
                                      r -> r

createGrid :: Int -> [Cell] -> Grid
createGrid width = M.fromList . zip cellsPos
  where cellsPos = [Pos (x-1) (y-1) | x <- [1..width], y <- [1..width]]

cellNextState :: Cell -> Int -> Cell
cellNextState cell n = case n of
                         2 -> cell
                         3 -> Alive
                         _ -> Dead

countAliveNeighbours :: Grid -> Pos -> Int
countAliveNeighbours grid pos = length $ filter isAlive $ mapMaybe (flip M.lookup grid) neighboursPos
  where neighboursPos = [ Pos (x + getX pos) (y + getY pos) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

nextGeneration :: Grid -> Grid
nextGeneration grid = M.mapWithKey (flip evolve) grid
  where evolve v = cellNextState v . countAliveNeighbours grid
