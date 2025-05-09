-- https://www.curry-lang.org/docs/tutorial/html/curry-tutorial.Ch3.S15.html
filelen :: IO ()
filelen = do
    filename <- getLine
    content <- readFile filename
    putStrLn (show (length content))

length :: String -> Int
length ""       = 0
length (_:xs)   = 1 + length xs

main = filelen
