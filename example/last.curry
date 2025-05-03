last :: Data a => [a] -> a
last [] = error "Empty list"
last (ys ++ [e]) = e

main = last [1, 2, 3, 4, 5]