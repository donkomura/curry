data Nat = O | S Nat

add :: Nat -> Nat -> Nat
add x y = add' (ensureNotFree x) y

add' O n     = n 
add' (S m) n = S (add' m n)
-- add :: Nat -> Nat -> Nat

isNat :: Nat -> Success
isNat O     = success
isNat (S n) = isNat n

goal = let x free in add x O =:= S O & isNat x

main = do
    let n1 = S (S O) -- 2
    let n2 = S (S (S O)) -- 3
    let n3 = add n1 n2 -- 5
    print(isNat n3)
    print(goal)