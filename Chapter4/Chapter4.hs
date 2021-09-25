module Chapter4.Chapter4 where

fizzbuzzIf :: (Integral a, Show a) => a -> [Char]
fizzbuzzIf n =
  if n `mod` 15 == 0
    then "fizzbuzz"
    else
      if n `mod` 5 == 0
        then "buzz"
        else
          if n `mod` 3 == 0
            then "fizz"
            else show n

fizzbuzzGuards :: (Integral a, Show a) => a -> [Char]
fizzbuzzGuards n
  | n `mod` 15 == 0 = "fizzbuzz"
  | n `mod` 5 == 0 = "buzz"
  | n `mod` 3 == 0 = "fizz"
  | otherwise = show n

addVectorsLet :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addVectorsLet v1 v2 =
  let (x1, y1) = v1
      (x2, y2) = v2
   in (x1 + x2, y1 + y2)

addVectorsWhere :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addVectorsWhere v1 v2 =
  (x1 + x2, y1 + y2)
  where
    (x1, y1) = v1
    (x2, y2) = v2

addVectorsCase :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addVectorsCase v1 v2 =
  case (v1, v2) of
    ((x1, y1), (x2, y2)) -> (x1 + x2, y1 + y2)