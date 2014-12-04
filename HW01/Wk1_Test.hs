module Wk1_Test where

import Wk1(validate, hanoi)
import Test.HUnit

-- Test Credit Card Validation
creditCardValidationCases = TestLabel "Credit Card Test Cases" ( TestList [
  testValid, testInvalid ] )

testValid = TestCase $ assertEqual
  "valid cards return True" True ( validate 4012888888881881 )
testInvalid = TestCase $ assertEqual 
  "invalid cards return False" False ( validate 4012888888881882 )

-- Test Hanoi
hanoiCases = TestLabel "Tower of Hanoi Three Peg Test Cases" ( TestList [
  testHanoiNoDiscs, testHanoiOneDisc, testHanoiTwoDiscs ] )

testHanoiNoDiscs = TestCase $ assertEqual
  "empty set of moves for no discs"  [] (
    hanoi 0 "a" "b" "c"
  )
testHanoiOneDisc = TestCase $ assertEqual
  "valid moves for one disc"  [("a","b")] (
    hanoi 1 "a" "b" "c"
  )
testHanoiTwoDiscs = TestCase $ assertEqual
  "valid moves for two disc"  [("a","c"), ("a","b"), ("c","b")] (
    hanoi 2 "a" "b" "c"
  )

main = runTestTT $ TestList [ creditCardValidationCases, hanoiCases ]
