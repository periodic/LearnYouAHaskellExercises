## 3. Types and Typeclasses
​
1. You can figure out what a lot of functions do just from their types.  In fact, the more general a type is, the less flexible the implementation is.  See how many implementations you can think of for the following.
   - The function that takes anything but always returns a `String`: `a -> String`
   - The function that takes anything and always returns a value of the same type: `a -> a`
   - The function that takes a function, `a -> b`, a list of `a` and returns a list of `b`: `(a -> b) -> [a] -> [b]`
   - The function that takes a function, `a -> String`, a list of `a` and returns a `String`: `(a -> String) -> [a] -> String`
   - The function `a -> Void`.  `Void` is the type that has no values.  So this function takes in a value and has no valid return values.
2. A couple interesting functions for dealing with types are ones that help you manipulate the inputs of functions so they can be used in different places.  How would you implement functions with the following types and how might you use them?
   - `flip :: (a -> b -> c) -> (b -> a -> c)`
   - `curry :: ((a, b) -> c) -> (a -> b -> c)`
   - `uncurry :: (a -> b -> c) -> ((a, b) -> c)`