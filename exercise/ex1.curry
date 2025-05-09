-- https://www.curry-lang.org/docs/tutorial/html/curry-tutorial.Ch3.S2.html
infixl 7 ./.
a ./. b = factors a b

factors :: Int -> Int -> Success
factors a b = b /= 0 && a `mod` b == 0

main = do
    print (10 ./. 0) -- should be False
    print (10 ./. 2) -- should be True
