from n = n : from(n + 1)
-- from :: Int -> [Int]

fibs = fibgen 1 1
fibgen n1 n2 = n1 : fibgen n2 (n1 + n2)
-- fibs :: [Int]
-- fibgen :: Int -> Int -> [Int]

primes = seive (from 2)
seive (x:xs) = x : seive (filter (\y -> y `mod` x > 0) xs)
-- prime :: [Int]
-- seive :: [Int] -> [Int]

ordMerge (x:xs) (y:ys) | x == y = x:ordMerge xs ys
                       | x < y  = x:ordMerge xs (y:ys)
                       | x > y  = y:ordMerge (x:xs) ys
hamming = 1:ordMerge (map (*2) hamming)
                     (ordMerge (map (*3) hamming)
                               (map (*5) hamming))

main = do
    print (take 10 (from 1))
    print (take 10 fibs)
    print (take 10 primes)
    print (take 10 hamming)