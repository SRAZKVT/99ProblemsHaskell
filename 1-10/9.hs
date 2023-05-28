pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x : xs) = packAux (x : xs) x
  where
    packAux :: Eq a => [a] -> a -> [[a]]
    packAux [] _ = []
    packAux xs curr =
      let h = takeWhile (== curr) xs
          t = dropWhile (== curr) xs
       in h : packAux t (head t)
