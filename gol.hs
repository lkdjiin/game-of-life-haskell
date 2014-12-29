import System.Random
import GameOfLife

main :: IO()
main =
    let width = 80
        height = 24
        cells = randomCells (width * height) (mkStdGen 123)
        generation = createGeneration width cells
     in putStrLn $ formatGeneration generation
