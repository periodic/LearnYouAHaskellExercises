module Chapter8.Json where

import qualified Data.Map as Map

data JsonValue
  = JsonString String
  | JsonNumber Float 
  | JsonNull
  | JsonArray [JsonValue]
  | JsonObject (Map.Map String JsonValue)
  deriving (Show, Eq, Read)

