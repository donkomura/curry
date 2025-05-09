-- https://www.curry-lang.org/docs/tutorial/html/curry-tutorial.Ch3.S5.html
data Task = Cut | Polish
data Worker = Alex | Bert | Chuck
    deriving Eq

-- assign task x to workers
assign :: Task -> Worker
assign Cut = Alex
assign Cut = Bert
assign Polish = Bert
assign Polish = Chuck

assigns | x /= y = (x, y)
    where x = assign Cut
          y = assign Polish

main = assigns

