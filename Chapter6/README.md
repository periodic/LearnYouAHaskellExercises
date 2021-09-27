## Chapter 6

1. Sieve of Eratosthones
    1. Write a function `isPrime :: Integral a => a -> Bool` that calculates if a number is prime or not.  Do this by writing a recursive function that takes a number `n` and then checks whether it can be divided by each number from 2 until the square-root of `n`.
        - Note that `sqrt` only works on `Floating` numbers, so you may wish to use `x*x > n` instead. 
    2. Now write a function `primes` using a filter on an infinite list to generate an infinite list of the prime numbers!
    3. We can do better! Instead of checking each number in sequence for divisibility we really only need to check the primes.  That's exactly what we are making!  Rewrite `isPrimes` function using `foldr` and iterate over `primes`.  
        - Remember, you must use `foldr` for an infinite list.
        - You'll also need to "prime the pump" by explicitly coding in a case for 2.
2. 