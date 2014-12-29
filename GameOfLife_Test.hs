module GameOfLife_Test where

import GameOfLife(cellNextState)
import Test.HUnit

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

main = runTestTT $ TestList [testCellNextState3,
                            testCellNextState4AndAlive,
                            testCellNextState4AndDead,
                            testCellNextState6]
