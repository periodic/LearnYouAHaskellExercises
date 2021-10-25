# Chapter 8

This chapter's exercises will be a little different.  Instead of a few little questions we are going to work through a larger project: building our own JSON library.

I've started by defining some data types for you.  These are a simplified in-memory representation of [JSON](https://en.wikipedia.org/wiki/JSON).  It's good enough for our purposes.  You can use it by importing `Json.hs`.  Here's the data type.

```haskell
data JsonValue
  = JsonString String
  | JsonNumber Float 
  | JsonNull
  | JsonArray [JsonValue]
  | JsonObject (Map.Map String JsonValue)
```

Short and sweet, right?  It's a recursive data structure that uses some of the types and containers that we are already familiar with.  It's also already a member of the `Eq`, `Show` and `Read` type classes.

## Part 1: Make it pretty

First we'll get used to the data by writing a pretty-printer. This will take our data structure and print it out nicely so that other people and computers can read it.  We'll be writing a function `prettyPrint` which handles the serialization and indentation.

```
*Chapter8.Chapter8> putStrLn . prettyPrint . JsonObject . Map.fromList $ [("name", JsonString "periodic"), ("likes", JsonArray [JsonString "Haskell", JsonNumber 42.0])]
{
  "likes": [
    "Haskell",
    42.0
  ],
  "name": "periodic"
}

```

A couple of notes:
- We won't worry about string escaping.  String manipulation can be fun, but we have other things to focus on.
- If a collection (object or array) is empty, we will print it on the same line, but if it has any elements they should be each on their own line and indented.
- Indentation will be two spaces.
- We'll print trailing zeros on whole-numbers because that's the default when using `show` on a `Float`.
- We'll use `String` concatenation which isn't very memory or performance efficient.  In a serious implementation you would use `ByteString` or `Text`.

There are tests written in `Chapter8/Tests.hs`.  You can load these in GHCi and update the imports for `Json.hs` and `Chapter8.hs` then call `failedPrinterTests` to view which tests failed.

The data structure is recursive and we need to handle indentation, so we'll want to write a recursive function.  We'll want to hide the indentation parameter, so you'll want to write a small wrapper that calls your workhorse function with `0` indent.

Tips:
- You can use deep pattern matching to make `JsonArray []` and `JsonArray elements` be separate cases.
- You will probably end up with a workhorse function that has 6-8 cases, possibly some guards, and then a handful of helper functions as you find them convenient.
- Remember not to indent your opening brace/bracket, but you will need to indent the closing one.

**Exercise 1.1**: Write an implementation of the `prettyPrint` function described above.

## Part 2: Parsing

Now we can read the JSON nicely we can start teaching the program how to read and understand JSON. We call this process parsing because we interpret one shape of data (a `String`) into another (`JsonValue`).  This is different than validation which just tests if data is in a desired format.  We'll focus on learning how to build parsers first, then adapt it to JSON specifically in the next section.

There are two key data types that we need to understand.  The first is a result type.  We want something that will hold the state of the parser, whether it is successful or not and what input remains to be parsed.

```haskell
data Result a
  = Success
      { value :: a,
        remaining :: String
      }
  | Failure
      { message :: String
      }
  deriving (Show, Eq)
```

This type has two types of members.  The first is `Success` which holds a parsed value and the remaining string left to be parsed.  The other is `Failure` which indicates that parsing has failed and holds a message as to why. You might notice that this could just have easily been `Either String (a, String)`, but it's easier to read if we give it clear names and will be easier to change or add to later.

**Exercise 2.1**: Take this type and create a `Functor` instance for it.

Next we should describe what parsing is.  It's fundamentally a function that takes in a `String` of input and produces a `Result a`.  So our `Parser` type will be defined as,

```haskell
type Parser a = String -> Result a
```

**Exercise 2.2**: Write a function `char :: Char -> Parser Char` which succeeds if the first character of the string matches the one given and fails otherwise.
```haskell
(char 'a') "abc" == Success 'a' "bc"
(char 'x') "abc" == Failure "Expected x"
```

Now that we have a type for our parser we need to learn how to compose parsers so that we can take small building blocks and combine them into more interesting parser.  We'll define a few functions that combine parsers together.  These types of functions are often called combinators.

**Exercise 2.3**: Write a function `mapParser` that is like `fmap` but for the `Parser` type.

**Exercise 2.4**: Write a function `andThen` which takes two arguments.  The first is a value of type `Parser a` and the second is a function `a -> Parser b` and has a final type of `Parser b`.  The resulting parser should be one that runs the first parser and if it succeeds then it passes the value and the remaining text to the second parser.  If the first parser fails then the function is never invoked and the original failure message is returned (you will have to re-wrap it in a `Failure` to match the types.)

These two functions, `mapParser` and `andThen` are sufficent to express just about any parsing you might want to do.  There are a few more utilities that we ought to write that will be very helpful going forward.  We are going to implement them as operators because it will make our code look cleaner.

**Exercise 2.5**: Write a pair of operators that will run two parsers but only return the left or right result.  If either parser fails then the whole expression should fail.  You can use the `andThen` function to invoke one parser and then the other.
```haskell
(<|) :: Parser a -> Parser b -> Parser a
(|>) :: Parser a -> Parser b -> Parser b

(char 'a' <| char 'b') "abc" == Success 'a' "c"
(char 'a' |> char 'b') "abc" == Success 'b' "c"
```

**Exercise 2.6**: Write an operator `(<|>)` that takes two parsers and returns both results as a tuple.  If either parser fails then the whole expression should fail.
```haskell
(<|>) :: Parser a -> Parser b -> Parser (a, b)

(char 'a' <|> char 'b') "abc" == Success ('a', 'b') "c"
```

With these combinators we are just about set.  We can now write functions that will parse a few characters, but that's of limited use.  Let's add a few more functions to make it easier to write larger parsers.  

**Exercise 2.7**: Write a function `optional :: Parser a -> Parser (Maybe a)`.  This combinator will create a parser that will return a `Just` value if the parser in the argument succeeds and a `Nothing` otherwise.  It must not consume any of the input if the parser fails.
```haskell
optional (char 'a') "abc" == Success (Just 'a') "bc"
optional (char 'x') "abc" == Success Nothing "abc"
```

**Exercise 2.8**: Write a function `while :: (Char -> Bool) -> Parser String` that will consume as much input as possible that matches the predicate.  Use `Data.List.span` to easily split the input at the right spot.
```haskell
while (Data.Char.isDigit) "123abc" == Success "123" "abc"
while (Data.Char.isDigit) "abc" == Success "" "abc"
```

**Exercise 2.9**: Write a function `many :: Parser a -> Parser [a]` that will repeat a parser over and over as many times as it can and returns all the results.
```haskell
many (while (Data.Char.isDigit) <| char ',') "abc" == Success "" ""
many (while (Data.Char.isDigit) <| char ',') "1,2,3,4" == Success ["1","2","3"] "4"
```

You may have noticed that that last example isn't quite what we would want.  The string `"1,2,3,4"` has a separator that isn't repeated as many times as the elements!  This is very common in parsing, so let's write another function for that pattern

**Exercise 2.10**: Write a function `sepBy :: Parser a -> Parser b -> Parser [a]`.  This function will find as many `a` values as it can, looking for a `b` in between each.  You can do this by trying to parse a `a` and then parsing `many (b |> a)` and combining the results.
```haskell
(while Data.Char.isLetter `sepBy` char ',') "lots,of,words" == Success ["lots", "of", "words"] ""
(char 'a' `sepBy` char ',') ",a" == Success [] ",a"
```

If you play around with the `sepBy`, `many` and `while` combinators you may notice some interesting behavior.  You can get some bad results if you combine them with parsers that can succeed without consuming any characters, which all of these can!  To get around this you'll want to make sure that the parser always consumes at least one iteration by explicitly parsing the first one and then letting `many` take care of the rest, much like you probably did for `sepBy`!

A few notes before we go.  While this parser system is easy to get started with, there are a lot of things you would want to take into account in a real-world scenario.

- `String` is inefficient and you would want to use `Text` or `ByteString` which store and access text much more efficiently.
- You may want to add a third state to differentate a complete successful parse from a partial consumption of input.
- This parser will fully back-track whenever it encounters an error, but you may actually want to consume some text on an error so that you can continue parsing and accumulate multiple errors.  For example, many compilers do this in some cases.

## Part 3: Parsing JSON