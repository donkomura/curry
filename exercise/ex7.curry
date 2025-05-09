-- https://www.curry-lang.org/docs/tutorial/html/curry-tutorial.Ch4.S2.html
accumulate :: [Int] -> Int
accumulate []       = 0
accumulate (x:xs)   = x + accumulate xs

main :: Int
main = accumulate [1,2,3,4,5]
