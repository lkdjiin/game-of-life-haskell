module GameOfLife_Test where

import GameOfLife(cellNextState, extractNeighborhood)
import Test.HUnit

cellNextStateTests = TestList [testCellNextState3,
                               testCellNextState4AndAlive,
                               testCellNextState4AndDead,
                               testCellNextState6]

testCellNextState3 = TestCase $ assertEqual
  "Gets 1 when neighborhood's sum is 3"
  1 (cellNextState 0 [1,1,1,0])

testCellNextState4AndAlive = TestCase $ assertEqual
  "Gets 1 when neighborhood's sum is 4 and cell is alive"
  1 (cellNextState 1 [1,1,1,0,1])

testCellNextState4AndDead = TestCase $ assertEqual
  "Gets 0 when neighborhood's sum is 4 and cell is dead"
  0 (cellNextState 0 [1,1,1,0,1])

testCellNextState6 = TestCase $ assertEqual
  "Gets 0 when neighborhood's sum is 6"
  0 (cellNextState 1 [1,1,1,0,1,1,1])

extractTests = TestList [testExtractSimpleCase,
                        testExtractLeft,
                        testExtractRight,
                        testExtractTop,
                        testExtractBottom,
                        testExtractTopLeft]
generation = [[1,2,3,4],
              [5,6,7,8],
              [9,10,11,12]]

testExtractSimpleCase = TestCase $ assertEqual
  "Gets the neighborhood"
  [1,2,3,5,6,7,9,10,11] (extractNeighborhood generation 1 1)

testExtractLeft = TestCase $ assertEqual
  "Gets the neighborhood when fetching left"
  [1,2,5,6,9,10] (extractNeighborhood generation 1 0)

testExtractRight = TestCase $ assertEqual
  "Gets the neighborhood when fetching right"
  [3,4,7,8,11,12] (extractNeighborhood generation 1 3)

testExtractTop = TestCase $ assertEqual
  "Gets the neighborhood when fetching top"
  [1,2,3,5,6,7] (extractNeighborhood generation 0 1)

testExtractBottom = TestCase $ assertEqual
  "Gets the neighborhood when fetching bottom"
  [5,6,7,9,10,11] (extractNeighborhood generation 2 1)

testExtractTopLeft = TestCase $ assertEqual
  "Gets the neighborhood when fetching top left"
  [1,2,5,6] (extractNeighborhood generation 0 0)

main = runTestTT $ TestList [cellNextStateTests, extractTests]
