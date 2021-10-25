module Chapter8.Tests where

import qualified Data.Map as Map
import Data.Bifunctor (first)

import Chapter8.Chapter8
import Chapter8.Json

failedPrinterTests :: IO ()
failedPrinterTests =
  mapM_ (uncurry printTest) . filter (not . uncurry (==)) . map (first prettyPrint) $ printerTests
  where
    printTest :: String -> String -> IO ()
    printTest actual expected = do
      putStrLn "Actual:"
      putStrLn actual
      putStrLn "Expected:"
      putStrLn expected

printerTests :: [(JsonValue, String)]
printerTests =
  [
    (JsonNull, "null"),
    (JsonString "", "\"\""),
    (JsonString "a string", "\"a string\""),
    (JsonNumber 0, "0.0"),
    (JsonNumber 123.456, "123.456"),
    (JsonArray [], "[]"),
    (JsonArray [JsonNull, JsonString "a string", JsonNumber 123.456], "[\n  null,\n  \"a string\",\n  123.456\n]"),
    (JsonArray [JsonArray [JsonArray []]], "[\n  [\n    []\n  ]\n]"),
    (JsonObject Map.empty, "{}"),
    (JsonObject $ Map.fromList [
        ("null", JsonNull),
        ("string", JsonString "string"),
        ("number", JsonNumber 123.456)
    ], "{\n  \"null\": null,\n  \"number\": 123.456,\n  \"string\": \"string\"\n}"),
    (JsonObject . Map.singleton "foo" . JsonObject . Map.singleton "bar" . JsonObject $ Map.empty, "{\n  \"foo\": {\n    \"bar\": {}\n  }\n}")
  ]

failedParserTests :: IO ()
failedParserTests =
  mapM_ (uncurry printTest) . filter (not . uncurry (==)) $ parserTests
  where
    printTest :: Result JsonValue -> Result JsonValue -> IO ()
    printTest actual expected = do
      putStrLn "Actual:"
      print . fmap prettyPrint $ actual
      putStrLn "Expected:"
      print . fmap prettyPrint $ expected

parserTests :: [(Result JsonValue, Result JsonValue)]
parserTests =
  [
    
    -- Null
    (jsonNull "null", Success JsonNull ""),
    (jsonNull "nul", Failure "Expected l"),
    (jsonNull "nll", Failure "Expected u"),
    
    -- String
    (jsonString "\"\"", Success (JsonString "") ""),
    (jsonString "\"string\"", Success (JsonString "string") ""),
    (jsonString "\"a more complicated string.\"", Success (JsonString "a more complicated string.") ""),
    (jsonString "\"not a string", Failure "Expected \""),

    -- Number
    (jsonNumber "123.456", Success (JsonNumber 123.456) ""),
    (jsonNumber "123.", Success (JsonNumber 123.0) "."),
    (jsonNumber "123.456.", Success (JsonNumber 123.456) "."),
    (jsonNumber "123e10", Success (JsonNumber 123.0) "e10"),

    -- Array
    (jsonArray "[]", Success (JsonArray []) ""),
    (jsonArray "[  ]", Success (JsonArray []) ""),
    (jsonArray "[[[]]]", Success (JsonArray [JsonArray [JsonArray []]]) ""),
    (jsonArray "[  null , \"string\" , 123.456 , [] , {} ]", Success (JsonArray [JsonNull, JsonString "string", JsonNumber 123.456, JsonArray [], JsonObject Map.empty]) ""),
    (jsonArray "[[[]", Failure "Expected ]"),

    -- Object
    (jsonObject "{}", Success (JsonObject Map.empty) ""),
    (jsonObject "{  }", Success (JsonObject Map.empty) ""),
    (jsonObject "{\"foo\":\"bar\"}", Success (JsonObject $ Map.singleton "foo" (JsonString "bar")) ""),
    (
      jsonObject "{ \"null\": null , \"string\": \"string\" , \"number\": 123.456 , \"array\": [] , \"object\": {} }",
      Success (JsonObject . Map.fromList $ [
        ("null", JsonNull),
        ("string", JsonString "string"),
        ("number", JsonNumber 123.456),
        ("array", JsonArray []),
        ("object", JsonObject Map.empty)
      ]) ""
    )
  ]
