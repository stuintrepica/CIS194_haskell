module HW02_Test where

import HW02
import Test.HUnit

-- Test Scrabble
validScrabbleCases = TestLabel "Word found in hand cases" ( TestList [
  someTiles, allTiles, emptyWord ] )

someTiles = TestCase $ assertEqual
  "a word formed with some of the tiles return True" True ( formableBy "fun" ['x', 'n', 'i', 'f', 'u', 'e', 'l'] )
allTiles = TestCase $ assertEqual 
  "a word formed with all tiles return True" True ( formableBy "haskell" ['k','l','e','h','a','l','s'] )
emptyWord = TestCase $ assertEqual
  "an empty word return true" True ( formableBy "" ['k','l','e','h','a','y','s'] )

invalidScrabbleCases = TestLabel "Can't make word cases" ( TestList [
  notFormable, emptyHand ] )

notFormable = TestCase $ assertEqual
  "a word not formable using all tiles" False ( formableBy "haskell" ['k','l','e','h','a','y','s'] )
emptyHand = TestCase $ assertEqual
  "an empty hand return false" False ( formableBy "word" [] )


wordsFormableCases = TestLabel "Words formable from a hand" ( TestList [
  basicHand, helloHand ] )

basicHand = TestCase $ assertEqual
  "words made from basic hand" ["ab","ad","ba","bad","cab","cad","dab"] ( wordsFrom ['a','b','c','d'] )
helloHand = TestCase $ assertEqual
  "words made from 'hello' hand" [ "eh","el","ell","he","hell","hello","helo","ho","hoe","hole","lo","oe","oh","ole" ] (
  wordsFrom ['h','e','l','l','o'] )

templateCases = TestLabel "Words fitting template" ( TestList [
  careTemplateTrue, 
  careTemplateFalse, 
  carTemplateFalse, 
  emptyHandTemplateFalse, 
  filledTemplateTrue,
  allWordsFittingTemplate ] )

careTemplateTrue = TestCase $ assertEqual
  "make care from hand and template" True ( wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "care" )
careTemplateFalse = TestCase $ assertEqual
  "can't make care from hand and template" False ( wordFitsTemplate "??r?" ['c','x','e','w','b','c','l'] "care" )
carTemplateFalse = TestCase $ assertEqual
  "can't make car from hand and template" False ( wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "car" )
emptyHandTemplateFalse = TestCase $ assertEqual
  "empty hand returns false" False ( wordFitsTemplate "??r" [] "car" )
filledTemplateTrue = TestCase $ assertEqual
  "full template return true" True ( wordFitsTemplate "let" ['x','x'] "let" )
allWordsFittingTemplate = TestCase $ assertEqual
  "all words that match template" ["acre","bare","carb","care","carl","earl"] ( wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l'] )

valueCases = TestLabel "word values" ( TestList [
  careValue, quizValue ] )

careValue = TestCase $ assertEqual
  "value for word care" 6 ( scrabbleValueWord "care" )
quizValue = TestCase $ assertEqual
  "value for word quiz" 22 ( scrabbleValueWord "quiz" )

bestWordsCases = TestLabel "best words" ( TestList [
  catBat ] )

catBat = TestCase $ assertEqual
  "cat bat" ["cat", "bat"] ( bestWords ["cat", "rat", "bat"] )

gameTileCases = TestLabel "game tile score cases" ( TestList [
  gameTiles1, gameTiles2, gameTiles3 ] )

gameTiles1 = TestCase $ assertEqual
  "peace 1" 27 (scrabbleValueTemplate "?e??3" "peace")
gameTiles2 = TestCase $ assertEqual
  "peace 2" 24 (scrabbleValueTemplate "De?2?" "peace")
gameTiles3 = TestCase $ assertEqual
  "peace 3" 11 (scrabbleValueTemplate "??Tce" "peace")

main = runTestTT $ TestList [
  validScrabbleCases,
  invalidScrabbleCases,
  wordsFormableCases,
  templateCases,
  valueCases,
  bestWordsCases,
  gameTileCases ]
