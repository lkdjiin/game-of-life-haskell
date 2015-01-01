module GameOfLife_Test where

import GameOfLife
import Test.HUnit

cellNextStateTests = TestList [testCellNextState3,
                               testCellNextState4AndAlive,
                               testCellNextState4AndDead,
                               testCellNextState6]

testCellNextState3 = TestCase $ assertEqual
  "Given dead cell when 3 alive neighbours then come to life"
  Alive (cellNextState Dead 3)

testCellNextState4AndAlive = TestCase $ assertEqual
  "Given alive cell when 2 alive neighbours then stay alive"
  Alive (cellNextState Alive 2)

testCellNextState4AndDead = TestCase $ assertEqual
  "Given dead cell when 2 alive neighbours then stay dead"
  Dead (cellNextState Dead 2)

testCellNextState6 = TestCase $ assertEqual
  "Given dead cell when 6 alive neighbours then stay dead"
  Dead (cellNextState Alive 6)

extractTests = TestList [testExtractSimpleCase,
                        testExtractLeft,
                        testExtractRight,
                        testExtractTop,
                        testExtractBottom,
                        testExtractTopLeft]
generation = createGrid 4 [Alive, Dead, Alive, Dead,
                           Dead, Alive, Dead, Alive,
                           Alive, Dead, Dead, Alive]

testExtractSimpleCase = TestCase $ assertEqual
  "Gets the number of alive neighbours in a complete neighborhood"
  3 (countAliveNeighbours generation (Pos 1 1))

testExtractLeft = TestCase $ assertEqual
  "Gets the number of alive neighbours when fetching left"
  3 (countAliveNeighbours generation (Pos 1 0))

testExtractRight = TestCase $ assertEqual
  "Gets the number of alive neighbours when fetching right"
  2 (countAliveNeighbours generation (Pos 1 3))

testExtractTop = TestCase $ assertEqual
  "Gets the number of alive neighbours when fetching top"
  3 (countAliveNeighbours generation (Pos 0 1))

testExtractBottom = TestCase $ assertEqual
  "Gets the number of alive neighbours when fetching bottom"
  2 (countAliveNeighbours generation (Pos 2 1))

testExtractTopLeft = TestCase $ assertEqual
  "Gets the number of alive neighbours when fetching top left"
  1 (countAliveNeighbours generation (Pos 0 0))

nextGenTests = TestList [testNextGrid]
generation0 = createGrid 4 [Alive, Dead, Dead, Dead,
                            Dead, Alive, Alive, Dead,
                            Alive, Dead, Alive, Dead]

generation1 = createGrid 4 [Dead,Alive,Dead,Dead,
                            Alive,Dead,Alive,Dead,
                            Dead,Dead,Alive,Dead]

testNextGrid = TestCase $ assertEqual
  "Gets the next generation"
  generation1 (nextGeneration generation0)

main = runTestTT $ TestList [cellNextStateTests, extractTests, nextGenTests]
