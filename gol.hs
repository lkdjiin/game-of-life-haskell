import System.Random
import GameOfLife
import Control.Concurrent
import Data.List (intercalate)

displayGrid :: Grid -> IO()
displayGrid grid = putStrLn $ formatGrid grid

formatGrid :: Grid -> String
formatGrid grid =
  let rows = intercalate "\n" (map (concatMap show) grid)
   in map replaceChar rows

replaceChar :: Char -> Char
replaceChar '1' = '@'
replaceChar '0' = ' '
replaceChar c   = c

randomCells :: Int -> StdGen -> [Cell]
randomCells size grid = take size $ randomRs (0, 1) grid

loop 0 _ = return ()
loop n g =
 do
   displayGrid g
   threadDelay 1000000
   loop (n-1) (nextGrid g)

main :: IO()
main =
  let width = 80
      height = 23
      cells = randomCells (width * height) (mkStdGen 1234)
      grid = createGrid width cells
   in loop 40 grid
