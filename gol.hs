import System.Random
import GameOfLife
import Control.Concurrent
import qualified Data.Map as M

displayGrid :: Grid -> IO()
displayGrid grid = putStrLn $ formatGrid grid

formatGrid :: Grid -> String
formatGrid = map replaceChar . fst . M.foldlWithKey reducer ("", -1)
  where reducer (acc, l) (x, _) v = if x == l
                                    then (show v ++ acc, l)
                                    else (show v ++ "\n" ++ acc, x)

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
   loop (n-1) (nextGeneration g)

main :: IO()
main =
  let width = 3
      height = 3
      cells = randomCells (width * height) (mkStdGen 1234)
      grid = createGrid width cells
   in loop 40 grid
