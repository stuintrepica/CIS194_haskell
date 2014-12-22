module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (w:ws) h
  | w `notElem` h = False
  | w `elem ` h = formableBy ws (delete w h)

wordsFrom :: Hand -> [String]
wordsFrom h = filter (`formableBy` h) allWords

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ [] = True
wordFitsTemplate _ _ [] = False
wordFitsTemplate [] _ _ = False
wordFitsTemplate (t:ts) h (s:ss)
  | s == t = wordFitsTemplate ts h ss
  | (s `elem` h && t == '?') = wordFitsTemplate ts (delete s h) ss
  | otherwise = False

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate _ [] = []
wordsFittingTemplate t h = filter (\w -> wordFitsTemplate t h w) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord [] = 0
scrabbleValueWord w = foldl (+) 0 (map scrabbleValue w)

bestWords :: [String] -> [String]
bestWords [] = []
bestWords wordsList = filter (\w -> (scrabbleValueWord w) == maximum(map scrabbleValueWord wordsList)) wordsList

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate [] [] = 0
scrabbleValueTemplate t s = sum (zipWith (*) (map tileMultiplier t) (map scrabbleValue s)) * product (map wordMultiplier t) where
  tileMultiplier :: Char -> Int
  tileMultiplier t
    | t == 'D' = 2
    | t == 'T' = 3
    | otherwise = 1
  
  wordMultiplier :: Char -> Int
  wordMultiplier t
    | t == '2' = 2
    | t == '3' = 3
    | otherwise = 1
