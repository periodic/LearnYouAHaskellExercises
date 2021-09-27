module Chapter6.Chapter6 where

isPrimeSimple :: Integral a => a -> Bool
isPrimeSimple 1 = True
isPrimeSimple n = testDivisors 2
  where
    testDivisors d
      | d*d > n = True
      | n `mod` d == 0 = False
      | otherwise = testDivisors (d + 1)

primes :: (Integral a, Enum a) => [a]
primes =
  filter isPrime [1..]

isPrime :: Integral a => a -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime n =
  foldr 
    (\p acc -> 
      p*p > n || (n `mod` p /= 0 && acc)
    )
    True
    (tail primes)