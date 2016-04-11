module Euler.Problem16 where

import Data.Char

-- solution 1
digitSum x = sum [digitToInt n | n <- show x]
-- main = print $ digitSum (2^1000)

-- solution 2
sumDigits ::  Integer -> Integer
sumDigits n = sumDigits' n 0
    where sumDigits' 0 acc = acc
          sumDigits' n acc = sumDigits' (div n 10) (acc + (mod n 10))

main ::  IO ()
main = print $ sumDigits $ 2^1000

-- solution 3
digsum base = f 0 where
 f a 0 = a
 f a n = f (a+r) q where
  (q,r) = n `divMod` base

main = print $ digsum 16 255 -- "FF": 15 + 15 = 30

-- solution 4
toDigits :: Integer -> [Integer]
toDigits n
      | n < 0 = []
      | n < 10 = [n]
      | otherwise =  toDigits (n `div` 10) ++ [n `mod` 10]

main = print $ sum $ toDigits $ 2^1000
