{-# LANGUAGE RankNTypes #-}
module Chapter6.Chapter6Advanced where

import Prelude hiding (Either(..))
import Chapter6.Chapter6 (primes)
import GHC.Generics (V1)

-- Optional
----------------------------------------

type Optional a = forall b. (a -> b) -> b -> b

-- 2.1.a
some :: a -> Optional a
some v f def = f v

-- 2.1.b
none :: Optional a
none f def = def

-- 2.1.c
safeHead :: [a] -> Optional a
safeHead [] = none
safeHead (x : _) = some x

-- 2.1.d
showFirst :: Show a => [a] -> String
showFirst xs =
  let optionalHead = safeHead xs
   in optionalHead show "empty list"

-- Either
----------------------------------------

type Either a b = forall c. (a -> c) -> (b -> c) -> c

-- 2.2.a
left :: a -> Either a b
left a l r = l a

-- 2.2.b
right :: b -> Either a b
right b l r = r b

-- 2.2.c
eitherPrimeOrFactor :: Integer -> Either Integer Integer
eitherPrimeOrFactor 1 = left 1
eitherPrimeOrFactor n =
  foldr isPrime' (left n) (tail primes)
  where
    isPrime' p rest
      | p * p > n = left n
      | n `mod` p /= 0 = rest
      | otherwise = right p

showPrimesAndFactors :: [Integer] -> [String]
showPrimesAndFactors =
  let showPrime p = "Prime: " ++ show p
      showFactor f = "Factor: " ++ show f
      showEntry primeOrFactor =
        primeOrFactor showPrime showFactor
   in map (showEntry . eitherPrimeOrFactor)

-- Dict
----------------------------------------

type Dict k v = k -> Optional v

-- 2.3.a
empty :: Dict k v
empty k = none

-- 2.3.b
add :: Eq k => k -> v -> Dict k v -> Dict k v
add k v dict = newDict
  where
    newDict query
      | k == query = some v
      | otherwise = dict query

-- 2.3.c
remove :: Eq k => k -> Dict k v -> Dict k v
remove k dict = newDict
  where
    newDict query
      | k == query = none
      | otherwise = dict query

-- 2.3.d
countElems :: Eq k => [k] -> Dict k Integer
countElems =
  -- Weirdly, foldr doesn't seem to work here because it wants to force the
  -- free type variables to all be the same.
  addKeys empty
  where
    increment :: Eq k => k -> Dict k Integer -> Dict k Integer
    increment k dict = add k (dict k (+1) 1) dict
    addKeys :: Eq k => Dict k Integer -> [k] -> Dict k Integer
    addKeys dict [] = dict
    addKeys dict (k:ks) = addKeys (increment k dict) ks