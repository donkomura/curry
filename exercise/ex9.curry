-- https://www.curry-lang.org/docs/tutorial/html/curry-tutorial.Ch4.S2.html
fibonacci = (0,1):[(y,x+y) | (x,y) <- fibonacci]

main = take 10 fibonacci
