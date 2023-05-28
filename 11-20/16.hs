dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEveryAux xs n n
  where
    dropEveryAux :: [a] -> Int -> Int -> [a]
    dropEveryAux [] _ _ = []
    dropEveryAux (_ : xs') 1 cap = dropEveryAux xs' cap cap
    dropEveryAux (x' : xs') n cap = x' : dropEveryAux xs' (n - 1) cap
