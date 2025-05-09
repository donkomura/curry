sublist :: [Int] -> [Int]
sublist (_++[x]++y++[z]++_) | z == 2 * x = [x]++y++[z]

main = do
    let a = [3,6,1,2,3,4]
    sublist a
