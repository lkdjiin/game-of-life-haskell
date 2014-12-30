module Slice_Test where

import Slice(sliceAround)
import Test.HUnit

list = [0,1,2,3,4,5]

simpleCases = TestList [testInner, testIndex1, testIndex4]

testInner = TestCase $ assertEqual
  "Gets inner list"
  [1,2,3] (sliceAround 2 list)

testIndex1 = TestCase $ assertEqual
  "Gets lowest list"
  [0,1,2] (sliceAround 1 list)

testIndex4 = TestCase $ assertEqual
  "Gets highest list"
  [3,4,5] (sliceAround 4 list)

edgeCases = TestList [testFirstIndex, testLastIndex]

testFirstIndex = TestCase $ assertEqual
  "Gets list for first index"
  [0,1] (sliceAround 0 list)

testLastIndex = TestCase $ assertEqual
  "Gets list for last index"
  [4,5] (sliceAround 5 list)

main = runTestTT $ TestList [simpleCases, edgeCases]
