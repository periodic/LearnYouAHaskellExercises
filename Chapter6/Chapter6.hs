module Chapter6.Chapter6 where

-- Sieve of Eratosthones
----------------------------------------

-- 1.1
isPrimeSimple :: Integer -> Bool
isPrimeSimple 1 = True
isPrimeSimple n = testDivisors 2
  where
    testDivisors d
      | d * d > n = True
      | n `mod` d == 0 = False
      | otherwise = testDivisors (d + 1)

-- 1.2
primes :: [Integer]
primes =
  filter isPrime [1 ..]

-- 1.3
isPrime :: Integer -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime n =
  foldr isPrime' True (tail primes)
  where
    -- Checks if the number is divisible by this prime and the rest.
    -- Only goes up to the square root of n.
    isPrime' p indivisibleByRest =
      p * p > n || (n `mod` p /= 0 && indivisibleByRest)

-- Optional
----------------------------------------

-- 2.1.a
some :: a -> (a -> b) -> b -> b
some v = \f def -> f v

-- 2.1.b
none :: (a -> b) -> b -> b
none f def = def

-- 2.1.c
safeHead :: [a] -> (a -> b) -> b -> b
safeHead [] = none
safeHead (x : _) = some x

-- 2.1.d
showFirst :: Show a => [a] -> String
showFirst xs =
  let optionalHead = safeHead xs
   in optionalHead show "empty list"

-- Either
----------------------------------------

-- 2.2.a
left :: a -> (a -> c) -> (b -> c) -> c
left a l r = l a

-- 2.2.b
right :: b -> (a -> c) -> (b -> c) -> c
right b l r = r b

-- 2.2.c
eitherPrimeOrFactor :: Integer -> (Integer -> c) -> (Integer -> c) -> c
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

-- 2.3.a
empty :: p -> (a -> b) -> b -> b
empty k = none

-- 2.3.b
add :: Eq t => t -> a -> (t -> (a -> b) -> b -> b) -> t -> (a -> b) -> b -> b
add k v dict query
  | k == query = some v
  | otherwise = dict query

-- 2.3.c
remove :: Eq t => t -> (t -> (a -> b) -> b -> b) -> t -> (a -> b) -> b -> b
remove k dict query
  | k == query = none
  | otherwise = dict query

-- 2.3.d
countElems :: Eq k => [k] -> k -> (Integer -> Integer) -> Integer -> Integer
countElems =
  foldr increment empty
  where
    increment k dict = add k (dict k (+ 1) 1) dict