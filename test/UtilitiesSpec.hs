module UtilitiesSpec where

import           Test.HUnit
import           Utilities

runUtilitiesTests = runTestTT tests

tests = TestList [ TestLabel "Single Item" testSingleItem
                 , TestLabel "Two Items" testTwoItems
                 , TestLabel "Sorted" testSorted
                 , TestLabel "Characters" testWithCharacters ]

testSingleItem = TestCase (
  assertEqual "Test single item" [] (pairs [0]) )

testTwoItems = TestCase (
  assertEqual "Test 2 items" [(0,1)] (pairs [0,1]) )

testSorted = TestCase (
  assertEqual "Test output is sorted" [(0,1), (1,2), (2,3)] (pairs [3,2,1,0]) )

testWithCharacters = TestCase (
  assertEqual "Test with characters" [('A','B'), ('B','C'), ('C','D')] (pairs "BADC") )