repli :: [a] -> Int -> [a]
repli xs n = concat $ map (replicate n) xs
