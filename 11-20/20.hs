removeAt :: Int -> [a] -> (a, [a])
removeAt p xs = removeAtAux xs p []
  where
    removeAtAux :: [a] -> Int -> [a] -> (a, [a])
    removeAtAux (x : xs) 1 acc = (x, acc ++ xs)
    removeAtAux (x : xs) p acc = removeAtAux xs (p - 1) (acc ++ [x])
