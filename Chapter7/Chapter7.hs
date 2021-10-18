module Chapter7.Chapter7 where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char

-- 1

insert :: (Ord k) => k -> Map.Map k Bool -> Map.Map k Bool
insert k = Map.insert k True

delete :: (Ord k) => k -> Map.Map k Bool -> Map.Map k Bool
delete = Map.delete

member :: (Ord k) => k -> Map.Map k Bool -> Bool
member = Map.member

-- 3

charFrequency :: String -> Map.Map Char Int
charFrequency =
  foldr (uncurry Map.insert) Map.empty
  . map (\cs@(c:_) -> (c, length cs))
  . List.group
  . List.sort
  . filter Char.isAlphaNum

wordFrequency :: String -> Map.Map String Int
wordFrequency =
  foldr (uncurry Map.insert) Map.empty
  . map (\cs@(c:_) -> (c, length cs))
  . List.group
  . List.sort
  . map (filter Char.isAlphaNum)
  . words

-- 4

type Position = (Int, Int)

type Board = Map.Map Position Char

type Visited = Set.Set Position

readBoard :: String -> Board
readBoard =
  foldr
    ( \(y, l) board ->
        foldr (\(x, p) -> Map.insert (x, y) p) board . zip [1 ..] $ l
    )
    Map.empty
    . zip [1 ..]
    . lines

hasPath :: String -> Board -> Bool
hasPath target board =
  hasPathFrom (Map.keys board) Set.empty target board

adjacentPositions :: Board -> Position -> [Position]
adjacentPositions board (x, y) =
  filter (flip Map.member board) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y -1)]

hasPathFrom :: [Position] -> Visited -> String -> Board -> Bool
hasPathFrom _ _ "" _ = True
hasPathFrom starts visited (t : rest) board =
  let validStarts = filter (\p -> Map.lookup p board == Just t) starts
   in any (\p -> hasPathFrom (nextSteps p) (Set.insert p visited) rest board) validStarts
  where
    nextSteps =
      filter (not . flip Set.member visited) . adjacentPositions board
