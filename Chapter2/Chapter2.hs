module Chapter2.Chapter2 where

import Data.List (tails)

fizzbuzz :: Integer -> [String]
fizzbuzz max =
  [ if n `mod` 15 == 0
      then "fizzbuzz"
      else
        if n `mod` 5 == 0
          then "buzz"
          else
            if n `mod` 3 == 0
              then "fizz"
              else show n
    | n <- [1 .. max]
  ]

sumPairs :: Integer -> [Integer] -> [(Integer, Integer)]
sumPairs target numbers =
  [ (x, y)
    | t <- tails numbers,
      x <- [head t],
      y <- t,
      x /= y,
      x + y == target
  ]

sep :: [Integer] -> [Integer]
sep xs =
  [ product xs `div` x | x <- xs ]

sepWithZero :: [Integer] -> [Integer]
sepWithZero xs =
  [ product (fst xy) * product (snd xy)
  | xy <- zip ((tails . tail) xs) ((reverse . tails . tail . reverse) xs)]

fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)