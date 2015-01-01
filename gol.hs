import System.Random
import GameOfLife
import Control.Concurrent
import qualified Data.Map as M
import Control.Monad (forM_)
import Data.Function(on)
import Data.List(groupBy, intercalate)

displayGrid :: Grid -> IO()
displayGrid = putStrLn . formatGrid

formatGrid :: Grid -> String
formatGrid = intercalate "\n" . map displayLine . groupByLine . M.toList
  where groupByLine = groupBy ((==) `on` (fst . fst))
        displayLine = map (displayCell . snd)

displayCell :: Cell -> Char
displayCell 1 = '@'
displayCell 0 = ' '

randomCells :: StdGen -> [Cell]
randomCells = randomRs (0, 1)

main :: IO()
main =
  let width = 80
      height = 23
      cells = take (width * height) $ randomCells (mkStdGen 1234)
      grid = createGrid width cells
   in forM_ (take 40 $ iterate nextGeneration grid) $ \g -> do
                                                         displayGrid g
                                                         threadDelay 1000000
