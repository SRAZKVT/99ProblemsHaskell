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

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) $ pack xs
