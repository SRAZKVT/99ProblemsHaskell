slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice _ 1 1 = []
slice (x : xs) 1 k = x : slice xs 1 (k - 1)
slice (_ : xs) i k = slice xs (i - 1) k
