import System.Random
import GameOfLife
import Control.Concurrent

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
