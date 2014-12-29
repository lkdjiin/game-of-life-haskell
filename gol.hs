import System.Random
import Data.List

type Cell = Int

randomCells :: Int -> StdGen -> [Cell]
randomCells size gen = take size $ randomRs (0, 1) gen

createGeneration :: Int -> [Cell] -> [[Cell]]
createGeneration _ [] = []
createGeneration width cells = line:(createGeneration width rest)
  where (line, rest) = splitAt width cells

formatGeneration :: [[Cell]] -> String
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

main :: IO()
main =
    let width = 80
        height = 24
        cells = randomCells (width * height) (mkStdGen 123)
        generation = createGeneration width cells
     in putStrLn $ formatGeneration generation
