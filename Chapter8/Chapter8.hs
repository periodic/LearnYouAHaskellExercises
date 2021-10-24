module Chapter8.Chapter8 where

import Chapter8.Json
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

-- Part 1
----------------------------------------

prettyPrint :: JsonValue -> String
prettyPrint = prettyPrintWithIndent 0
  where
    indentStr :: Int -> String
    indentStr indent = replicate (indent * 2) ' '

    indentList :: Int -> [String] -> String
    indentList indent = List.intercalate ",\n" . map (indentStr indent ++)

    escape :: Char -> String
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c = [c]

    quote :: String -> String
    quote str =
      "\"" ++ concatMap escape str ++ "\""

    prettyPrintWithIndent :: Int -> JsonValue -> String
    prettyPrintWithIndent indent (JsonString str) = quote str
    prettyPrintWithIndent indent (JsonNumber num) = show num
    prettyPrintWithIndent indent JsonNull = "null"
    prettyPrintWithIndent indent (JsonArray []) = "[]"
    prettyPrintWithIndent indent (JsonArray values) =
      let bracketIndent = indentStr indent
          printedValues = map (prettyPrintWithIndent (indent + 1)) values
      in "[\n"
            ++ indentList (indent + 1) printedValues
            ++ "\n"
            ++ bracketIndent
            ++ "]"
    prettyPrintWithIndent indent (JsonObject fields)
      | Map.null fields = "{}"
      | otherwise =
        let bracketIndent = indentStr indent
            printedValues = map (\(k, v) -> quote k ++ ": " ++ prettyPrintWithIndent (indent + 1) v) . Map.assocs $ fields
        in "{\n"
              ++ indentList (indent + 1) printedValues
              ++ "\n"
              ++ bracketIndent
              ++ "}"

-- Part 2
----------------------------------------

data Result a
  = Success
      { value :: a,
        remaining :: String
      }
  | Failure
      { message :: String
      }
  deriving (Show, Eq)

instance Functor Result where
  f `fmap` (Success val remaining) = Success (f val) remaining
  f `fmap` (Failure msg) = Failure msg

type Parser a = String -> Result a

success :: a -> Parser a
success = Success

failure :: String -> Parser a
failure msg str = Failure msg

mapParser :: (a -> b) -> Parser a -> Parser b
mapParser f parser str =
  case parser str of
    Success a s ->
      Success (f a) s
    Failure s ->
      Failure s

andThen :: Parser a -> (a -> Parser b) -> Parser b
andThen parserA fParserB str =
  case parserA str of
    Success a s ->
      fParserB a s
    Failure s ->
      Failure s

(<|) :: Parser b1 -> Parser b2 -> Parser b1
pa <| pb =
  pa `andThen` \a -> mapParser (const a) pb

(|>) :: Parser b1 -> Parser b2 -> Parser b2
pa |> pb =
  pa `andThen` const pb

(<|>) :: Parser a -> Parser b -> Parser (a, b)
pa <|> pb =
  pa `andThen` \a ->
    pb `andThen` \b ->
      success (a, b)

char :: Char -> Parser Char
char target [] = Failure $ "Expected " ++ [target]
char target (c : cs)
  | c == target = Success c cs
  | otherwise = Failure $ "Expected " ++ [target]

while :: (Char -> Bool) -> Parser String
while pred str =
  uncurry Success $ List.span pred str

optional :: Parser a -> Parser (Maybe a)
optional parser str =
  case parser str of
    Success a rest ->
      Success (Just a) rest
    Failure _ ->
      Success Nothing str

many :: Parser a -> Parser [a]
many = many' []
  where
    many' results parser str =
      case parser str of
        Success a s ->
          many' (results ++ [a]) parser s
        Failure s ->
          Success results str

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep parser =
  mapParser (Maybe.fromMaybe []) $
    optional
      ( parser `andThen` \firstVal ->
          mapParser (firstVal :) (many (sep |> parser))
      )

oneOf :: [Parser a] -> Parser a
oneOf [] str =
  Failure "No patterns match"
oneOf (p : ps) str =
  case p str of
    success@(Success _ _) ->
      success
    Failure _ ->
      oneOf ps str

each :: [Parser a] -> Parser [a]
each [] str =
  Success [] str
each (p : ps) str =
  case p str of
    Success a remaining ->
      (mapParser (a :) $ each ps) remaining
    Failure msg ->
      Failure msg

string :: Parser String
string =
  char '\"' |> while (/= '\"') <| char '\"'

digit :: Parser Char
digit [] = Failure "Expected a number"
digit (c : cs)
  | Char.isDigit c = Success c cs
  | otherwise = Failure "Expected a number"

whitespace :: Parser String
whitespace = while Char.isSpace

jsonValue :: Parser JsonValue
jsonValue =
  whitespace
    |> oneOf
      [ jsonNull,
        jsonString,
        jsonNumber,
        jsonArray,
        jsonObject
      ]
    <| whitespace

jsonNull :: Parser JsonValue
jsonNull =
  mapParser (const JsonNull) $
    each [char 'n', char 'u', char 'l', char 'l']

jsonString :: Parser JsonValue
jsonString = mapParser JsonString string

jsonNumber :: Parser JsonValue
jsonNumber =
  mapParser (JsonNumber . read . concat) $
    each
      [ mapParser (: []) digit,
        while Char.isDigit,
        mapParser (concat . concat . Maybe.maybeToList) . optional $
          each
            [ mapParser (: []) (char '.'),
              mapParser (: []) digit,
              while Char.isDigit
            ]
      ]

jsonArray :: Parser JsonValue
jsonArray =
  mapParser JsonArray $
    char '[' |> whitespace |> sepBy (whitespace |> char ',' |> whitespace) jsonValue <| whitespace <| char ']'

jsonObject :: Parser JsonValue
jsonObject =
  mapParser (JsonObject . Map.fromList) $
    char '{' |> whitespace
      |> sepBy
        (whitespace |> char ',' |> whitespace)
        (string <|> (whitespace |> char ':' |> whitespace |> jsonValue))
      <| whitespace
      <| char '}'
