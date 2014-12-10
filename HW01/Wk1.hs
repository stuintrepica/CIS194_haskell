module Wk1(validate, hanoi) where

lastDigit :: Integer -> Integer
lastDigit 0 = 0
lastDigit x = x `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit y = y `div` 10

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits(dropLastDigit(n)) ++ [lastDigit(n)]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (p:ps)
  | (length(ps) `mod` 2 == 0) = p : doubleEveryOther ps
  | otherwise = (p*2) : doubleEveryOther ps

sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (+) 0 (concatMap toDigits xs)

validate :: Integer -> Bool
validate n = sumDigits(doubleEveryOther(toDigits n)) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n from to using = (hanoi (n-1) from using to) ++ [(from,to)] ++ (hanoi (n-1) using to from)
