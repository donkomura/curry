-- https://www.curry-lang.org/docs/tutorial/html/curry-tutorial.Ch3.S6.html
data List a = Element a (List a) | Sentinel

length Sentinel = 0
length (Element _ l) = 1 + length l

contain _ Sentinel = False
contain k (Element x l) = x == k || contain k l

type IntList = List Int

test1 :: IntList
test1 = Element 1 (Element 2 (Element 3 Sentinel))

assert got wanted | got == wanted = True
                  | otherwise     = False 

main = do
    print (assert (length test1) 3)
    print (assert (contain 2 test1) True)
    print (assert (contain 1000 test1) False)
