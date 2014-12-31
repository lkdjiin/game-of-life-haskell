import System.Random
import GameOfLife
import Control.Concurrent
import Data.List (intercalate)

displayGeneration :: Generation -> IO()
displayGeneration generation = putStrLn $ formatGeneration generation

formatGeneration :: Generation -> String
formatGeneration generation =
  let rows = intercalate "\n" (map (concatMap show) generation)
   in map replaceChar rows

replaceChar :: Char -> Char
replaceChar '1' = '@'
replaceChar '0' = ' '
replaceChar c   = c

randomCells :: Int -> StdGen -> [Cell]
randomCells size generation = take size $ randomRs (0, 1) generation

loop 0 _ = return ()
loop n g =
 do
   displayGeneration g
   threadDelay 1000000
   loop (n-1) (nextGeneration g)

main :: IO()
main =
  let width = 80
      height = 23
      cells = randomCells (width * height) (mkStdGen 1234)
      generation = createGeneration width cells
   in loop 40 generation
