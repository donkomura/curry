split _ [] = ([], [])
split e (x:xs) | e >= x = (x:l,r)
               | e < x  = (l,x:r)
               where (l,r) = split e xs
-- split :: Ord a => a -> [a] -> ([a], [a])

qsort [] = []
qsort (x:xs) = let (l,r) = split x xs
               in qsort l ++ (x:qsort r)

split_last = split 10 [1, 2, 3, 4, 5]
split_middle = split 3 [1, 2, 3, 4, 5, 6]
goal = qsort [8,6,7,5,4,2,3,1]

main = do
        print split_middle
        print split_last
        print goal