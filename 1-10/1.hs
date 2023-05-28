myLast :: [a] -> a
myLast [] = error "No empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs
