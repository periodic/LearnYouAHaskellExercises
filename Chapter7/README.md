# Chapter 7

## 1. Roll Your Own Wheel

A `Set` can be modeled by using a map that stores `Bool` values.  Let's do it, for science!  Import `Data.Map` and write up your own versions of `insert`, `member` and `delete`.  You'll may also need to use the `Data.Map` versions of these functions so you'll have deal with qualified imports.

```haskell
insert :: (Ord k) => k -> Map k Bool -> Map k Bool
delete :: (Ord k) => k -> Map k Bool -> Map k Bool
member :: (Ord k) => k -> Map k Bool -> Bool
```

## 2. Hoogle From the Mountaintops

[Hoogle](https://hoogle.haskell.org/) is an invaluable tool for programming!  It lets you look up modules and functions by name, by type.  Let's try it out.

First, let's find some functions by name.

1. `permutations` - A function that gives you all the permutations of a list.
2. `Data.Text` - A module for dealing with Unicode text.

Next, let's look up a few functions by type.

1. A function that takes an `Int` and an element of type `a` and returns `[a]` which has the element repeated the specified number of times.
2. A function tht takes in a function `v -> v` and a key `k` and then a `Map k v` and returns a new map with the function applied to the value at `k` if present.

## 3. Stop Repeating Yourself

1. Let's do some frequency analysis.  You may have noticed the little function they wrote in the chapter to get the frequency of letters in a string.  Let's make that a little better by improving it in two ways.  First, let's filter it to just alpha-numeric characters so that we aren't counting all the punctuation and spaces.  Then let's also put it in a map for easy lookup!  This function can be written almost entirely without variables with clever use of functions like `uncurry`.
2. Now let's kick it up a notch and do word-frequncy analysis.  We'll need to use the `words` function and we still want to filter out all the punctuation and spaces.

## 4. Boggle The Mind

Boggle is a simple game played by looking at a grid of letters and trying to find words.  Let's write a function to check for words for us.

1. First, we need to build our grid.  Write a function that takes in a string describing the grid, with one line per row, and builds a map for easy look up.  The keys can be the coordinate, `(Int, Int)`, and the value should be the `Char`.  
    - Tip: The `lines` function will take care of the various lines.  
    - Tip: You can index a list using `zip [1..]`.    
    - Tip: You may want to think about how you would write this in an imperative program with nested loops.  You will need nested `foldr` functions.
2. Now that we have our board, let's see if we can figure out if a word is present.  Write a function that takes in a target `String` and the board and returns a `Bool` indicating whether the `String` can be traced starting from anywhere and moving to adjacent (not diagonal) squares.  
    You'll need a recursive helper function to do the work.  It'll take in a list of coordinates and check whether the target string can be built from that coordinate.  It'll recurse until either the list of coordinates is empty (`False`) or the string is empty (`True`).  It'll look something like  
    ```haskell
    hasPathFrom :: [(Int, Int)] -> String -> Map (Int, Int) Char -> Bool
    hasPathFrom [] _ _ = False
    hasPathFrom _ "" _ = True
    hasPathFrom starts target board = ...
    ```  
    You'll want to start out with every square on the board as possible positions.  You can get that by just fetching all the keys from the board.
    You may want to use the `any :: (a -> Bool) -> [a] -> Bool` function to combine different possible paths.
3. Take a deep breath.  You are doing great.  However, I misread the rules.  It turns out we can't visit the same square twice.  Add another argument to your helper function that is a `Set` of the visited coordinates and filter out coordinates that you've already visited on this path.

