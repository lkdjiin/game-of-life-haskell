module GameOfLife_Test where

import GameOfLife(cellNextState, extractNeighborhood, nextGeneration, createGrid)
import Test.HUnit

cellNextStateTests = TestList [testCellNextState3,
                               testCellNextState4AndAlive,
                               testCellNextState4AndDead,
                               testCellNextState6]

testCellNextState3 = TestCase $ assertEqual
  "Given dead cell when 3 alive neighbours then come to life"
  1 (cellNextState 0 3)

testCellNextState4AndAlive = TestCase $ assertEqual
  "Given alive cell when 2 alive neighbours then stay alive"
  1 (cellNextState 1 2)

testCellNextState4AndDead = TestCase $ assertEqual
  "Given dead cell when 2 alive neighbours then stay dead"
  0 (cellNextState 0 2)

testCellNextState6 = TestCase $ assertEqual
  "Given dead cell when 6 alive neighbours then stay dead"
  0 (cellNextState 1 6)

extractTests = TestList [testExtractSimpleCase,
                        testExtractLeft,
                        testExtractRight,
                        testExtractTop,
                        testExtractBottom,
                        testExtractTopLeft]
generation = createGrid 4 [1,2,3,4,
                           5,6,7,8,
                           9,10,11,12]

testExtractSimpleCase = TestCase $ assertEqual
  "Gets the neighborhood"
  [1,2,3,5,7,9,10,11] (extractNeighborhood generation 1 1)

testExtractLeft = TestCase $ assertEqual
  "Gets the neighborhood when fetching left"
  [1,2,6,9,10] (extractNeighborhood generation 1 0)

testExtractRight = TestCase $ assertEqual
  "Gets the neighborhood when fetching right"
  [3,4,7,11,12] (extractNeighborhood generation 1 3)

testExtractTop = TestCase $ assertEqual
  "Gets the neighborhood when fetching top"
  [1,3,5,6,7] (extractNeighborhood generation 0 1)

testExtractBottom = TestCase $ assertEqual
  "Gets the neighborhood when fetching bottom"
  [5,6,7,9,11] (extractNeighborhood generation 2 1)

testExtractTopLeft = TestCase $ assertEqual
  "Gets the neighborhood when fetching top left"
  [2,5,6] (extractNeighborhood generation 0 0)

nextGenTests = TestList [testNextGrid]
generation0 = createGrid 4 [1,0,0,0,
                            0,1,1,0,
                            1,0,1,0]

generation1 = createGrid 4 [0,1,0,0,
                            1,0,1,0,
                            0,0,1,0]

testNextGrid = TestCase $ assertEqual
  "Gets the next generation"
  generation1 (nextGeneration generation0)

main = runTestTT $ TestList [cellNextStateTests, extractTests, nextGenTests]
