-- https://www.curry-lang.org/docs/tutorial/html/curry-tutorial.Ch3.S10.html
class Addable a where
    (+) :: a ->  a  -> a

instance Addable Int where
    x + y = x Prelude.+ y

instance Addable a => Addable [a] where
    xs + ys = zipWith (+) xs ys

data Matrix a = Matrix [[a]]
instance Addable a => Addable (Matrix a) where
    (Matrix xs) + (Matrix ys) = Matrix (zipWith (zipWith (+)) xs ys)

main :: Matrix Int
main = Matrix [[1,2],[3,4]] + Matrix [[2,3],[4,5]]
