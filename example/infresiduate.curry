conc xs ys = append xs ys                  --> finite search space
--conc xs ys = append (ensureSpine xs) ys  --> infinite search space

append []     ys = ys
append (x:xs) ys = x : append xs ys

rev [] []     = success
rev l  (x:xs) = let lx free in conc lx [x] =:= l & rev lx xs

goal l = rev [1,2,3] l

main = do
    print (let l free in goal l & l =:= [3,2,1])