## Chapter 6

1. Sieve of Eratosthones
    1. Write a function `isPrime :: Integral a => a -> Bool` that calculates if a number is prime or not.  Do this by writing a recursive function that takes a number `n` and then checks whether it can be divided by each number from 2 until the square-root of `n`.
        - Note that `sqrt` only works on `Floating` numbers, so you may wish to use `x*x > n` instead. 
    2. Now write a function `primes` using a filter on an infinite list to generate an infinite list of the prime numbers!
    3. We can do better! Instead of checking each number in sequence for divisibility we really only need to check the primes.  That's exactly what we are making!  Rewrite `isPrimes` function using `foldr` and iterate over `primes`.  
        - Remember, you must use `foldr` for an infinite list.
        - You'll also need to "prime the pump" by explicitly coding in a case for 2.
2. Functional Data Types: Let's create some data types using higher order functions!  This is a really cool trick that has some important implications down the line.
    1. Let's start with optional values.  It's the type that can have a value or not.  How do you represent that?  Well the only way to _use_ an optional is to provide two cases: one for a present value and one for a missing value.  We can represent that as a function with the following type:  
    ` optional :: (a -> b) -> b -> b`  
        1. Write a function `some` that takes in a value and returns an `optional`, meaning it returns a function with the signature of `optional`.  When the `optional` is called it should evaluate the first argument of the optional with its value and return the result.  For example, `(some 1) (+1) 0 = 2`.
        2. Write a function `none` that is an optional that will always provide the second parameter when fully evaluated.  For example, `none (+1) 0 = 0`.
        3. Write a function `safeHead :: [a] -> (a -> b) -> b -> b`.  This should take in a list and return an optional representing whether the head could be found or not.
        4. Use your `safeHead` to write a function `showFirst :: Show a => [a] -> String` that will `show` the first element of a list if present and otherwise show `"empty list"`.
    2. `Optional` was pretty cool, let's do another!  Let's implement `either :: (a -> c) -> (b -> c) -> c`.
        1. Write a function `left :: a -> either` such that 
            ```haskell
            left 1 (show) (++ " functions") == "1"
            ```
        2. Write a function `right :: b -> either` such that
            ```haskell
            right "fancy" (show) (++ " functions") ==
                "fancy functions"
            ```
        3. Write a function like `isPrime` that returns either number if it is prime or the first factor of the number found.  Since these are both numbers you'll have to use `left` and `right` to differentiate them.  Use that function to map `[1..20]` to strings containing `"Prime: n"` if it is prime or `"Factor: f"` if it is a factor.
            - Note: the signature of your `isPrime` function should be `(Integer -> c) -> (Integer -> c) -> c` because both the left and right sides have the same type and you probably want to use `Integer` just to avoid specifying `Integral` and `Show` all the time.
    3. Nice!  Now let's implement a simple dictionary lookup system!  What is a dictionary other that a way to provide a key and check if a value is present or not?  That's pretty easy to type out.  It's just a function that takes a key `Eq k => k` and returns an optional value `v`.  Let's build it using recursive functions!  
    (This will be horribly inefficient, so don't do this in production.)
        1. Let's start with the empty dictionary.  This should be a function `empty` that will return `none` no matter what key is passed in.  This is our base/edge case. So in this case,  
            ```haskell
            empty k show "not found" == "not found"
            ```
        2. What good are dictionaries if we can't add things? Write a function `add` which takes a key, value and dictionary and produces a new dictionary that will return the value when queried with the key. It should behave such that  
            ```haskell
            let dict = add "a" 42 empty
            in dict "a" id 0 == 42
            ```
        3. Sometimes we like to remove things, so let's write `remove` which will cause the dictionary to return `none` if the key is provided.
            ```haskell
            let dict = remove "a" . add "a" 42 $ empty
            in dict "a" id 0 == 0
            ```
        4. Now let's do something a little more interesting.  Write a function `countElems` that will take in a list of items belonging to the `Eq` typeclass and return a dictionary with their counts.  For example,
            ```haskell
            (countElems [1, 2, 1, 3, 1]) 1 id 0 == 3
            ```
            - Note: If you explicitly write the types you will have to type this function as `countElem :: Eq k => [k] -> k -> (Integer -> Integer) -> Integer -> Integer`.  This is because we don't yet have the knowledge to type it properly.
