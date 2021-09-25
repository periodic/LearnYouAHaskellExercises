module Chapter5.Chapter5 where


mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort xs =
  let middle = length xs `div` 2
      (front, back) = splitAt middle xs
  in
    merge (mergeSort front) (mergeSort back)
  where
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | y > x = y : merge (x:xs) ys
      | otherwise = x : merge xs (y:ys)

maximum :: (Ord a) => [a] -> a
maximum [] = error "maximum of empty list"
maximum [x] = x
maximum (x:xs) = maximum' x xs
  where
    maximum' x [] = x
    maximum' x (y:ys) = maximum' (max x y) ys

replicate :: (Num i, Ord i) => i -> a -> [a]
replicate n x = replicate' [] n
  where
    replicate' xs n
      | n <= 0 = xs
      | otherwise = replicate' (x:xs) (n - 1)

fibonacci :: (Num n, Ord n) => n -> n
fibonacci n = fibonacci' n 0 1
  where
    fibonacci' n a b
      | n <= 0 = a
      | otherwise = fibonacci' (n - 1) b (a + b)