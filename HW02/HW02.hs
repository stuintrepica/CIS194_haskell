module HW02(formableBy, wordsFrom, wordFitsTemplate, wordsFittingTemplate) where

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
formableBy (w:ws) hand 
  | w `notElem` hand = False
  | w `elem ` hand = formableBy ws (delete w hand)

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ [] = True
wordFitsTemplate template _ [] = False
wordFitsTemplate [] _ _ = False
wordFitsTemplate (t:ts) hand (f:fs)
  | f == t = wordFitsTemplate ts hand fs
  | (f `elem` hand && t == '?') = wordFitsTemplate ts (delete f hand) fs
  | otherwise = False

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate _ [] = []
wordsFittingTemplate template hand = filter (\w -> wordFitsTemplate template hand w) allWords

