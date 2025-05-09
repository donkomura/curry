-- https://www.curry-lang.org/docs/tutorial/html/curry-tutorial.Ch4.S2.html
transpose :: [[Int]] -> [[Int]]
transpose a =
    let col = let firstCol []       = []
                  firstCol ([]:_)  = []
                  firstCol ((x:_):xs) = x : firstCol xs
                in firstCol a
        row = let restCol []        = []
                  restCol ([]:_)   = []
                  restCol ((_:x):xs) = x : restCol xs
                in restCol a
    in if row == [] then [] else col : transpose row

m = [[0,1,2],[3,4,5],[6,7,8]]
want = [[0,3,6],[1,4,7],[2,5,8]]
main = transpose m == want
