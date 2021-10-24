# Chapter 8

This chapter's exercises will be a little different.  Instead of a few little questions we are going to work through a larger project: building our own JSON library.

I've started by defining some data types for you.  These are a simplified in-memory representation of [JSON](https://en.wikipedia.org/wiki/JSON).  It's good enough for our purposes.  You can use it by importing `Chapter8.Json`.  Here's the data type.

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

There are tests written in `Chapter8/Tests.hs`.  You can load these in GHCi and call `failedPrinterTests` to view which tests failed.

The data structure is recursive and we need to handle indentation, so we'll want to write a recursive function.  We'll want to hide the indentation parameter, so you'll want to write a small wrapper that calls your workhorse function with `0` indent.

Tips:
- You can use deep pattern matching to make `JsonArray []` and `JsonArray elements` be separate cases.
- You will probably end up with a workhorse function that has 6-8 cases, possibly some guards, and then a handful of helper functions as you find them convenient.
- Remember not to indent your opening brace/bracket, but you will need to indent the closing one.
