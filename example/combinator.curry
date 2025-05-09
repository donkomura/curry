-- combinator
fix :: (a -> a) -> a
fix f = let x = f x in x

-- use it in factorial
factorial :: Int -> Int
factorial = fix (\f n -> if n == 0 then 1 else n * f (n - 1))

main = factorial 5

