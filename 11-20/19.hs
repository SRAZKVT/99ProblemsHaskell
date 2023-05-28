rotate :: Show a => [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n
  | n < 0 = rotate (last xs : init xs) (n + 1)
  | otherwise = rotate (tail xs ++ [head xs]) (n - 1)
