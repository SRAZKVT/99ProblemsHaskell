data Encoding a = Single a | Multiple Int a deriving (Show)

encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect xs =
  map
    ( \x ->
        ( case x of
            (1, x') -> (Single x')
            (n, x') -> (Multiple n x')
        )
    )
    $ asTuples xs

asTuples :: Eq a => [a] -> [(Int, a)]
asTuples [] = []
asTuples xs = (n, h) : asTuples (drop n xs)
  where
    h = head xs
    n = f xs h

f :: Eq a => [a] -> a -> Int
f [] curr = 0
f (x : xs) curr
  | x == curr = 1 + f xs curr
  | otherwise = 0
