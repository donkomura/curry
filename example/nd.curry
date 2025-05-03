-- choose is the basic non-deterministic choice function
choose x  _ = x
choose _  y = y

-- Non-deterministic insertion in a list
insert x []     = [x]
insert x (y:ys) = choose (x:y:ys) (y:insert x ys)

-- Non-deterministic generation of permutations
permut []     = []
permut (x:xs) = insert x (permut xs)

sort xs = rId sorted (permut xs)

wheresort xs | sorted ys = ys
    where ys = permut xs

structsort xs | ys =:= permut xs & sorted ys =:= True = ys where ys free

-- p x が true のとき x を返す
-- p x が false のとき評価は失敗する
rId :: (a -> Bool) -> a -> a
rId p x | p x = x

sorted [] = True
sorted [_] = True
sorted (x:y:ys) | x <= y = sorted(y:ys)

main = do
    let xs = [3, 2, 4, 1, 5, 9]
    let sorted = sort xs
    let wheresorted = wheresort xs
    let structsorted = structsort xs
    print sorted
    print wheresorted
    print structsorted
    print (sorted == wheresorted)
    print (sorted == structsorted)