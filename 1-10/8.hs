compress :: Eq a => [a] -> [a]
compress (x : xs) = compressAux xs x
  where
    compressAux :: Eq a => [a] -> a -> [a]
    compressAux [] curr = [curr]
    compressAux (x' : xs') curr
      | x' == curr = compressAux xs' x'
      | otherwise = curr : compressAux xs' x'
