append :: [t] -> [t] -> [t]

append []     x  = x
append (x:xs) ys = x : append xs ys

rev :: [a] -> [a]

rev []      = []
rev (x:xs)  = append (rev xs) [x]

goal1 = append [1,2] [3,4]
goal2 = rev [1,2,3,4]
goal3 = rev [1,2,3,4,5,6,7,8,9,10]

main
    = do
        print goal1
        print goal2
        print goal3