data Encoding a = Single a | Multiple Int a deriving (Show)

decodeModified :: Eq a => [Encoding a] -> [a]
decodeModified xs =
  concat $
    map
      ( \x ->
          ( case x of
              (Single x') -> [x']
              (Multiple p x') -> replicate p x'
          )
      )
      xs
