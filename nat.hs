
:{
data Nat where
    Zero :: Nat
    Succ :: Nat -> Nat
:}

add :: Nat -> Nat -> Nat
add n Zero     = n              -- n + 0 = n
add n (Succ m) = Succ (add n m) -- n + (m + 1) = (n + m) + 1


mul :: Nat -> Nat -> Nat
mul n Zero     = Zero            -- n * 0 = 0
mul n (Succ m) = add (mul n m) n -- n * (m + 1) = (n * m) + n


exp :: Nat -> Nat -> Nat
exp n Zero     = Succ Zero       -- n ^ 0 = 1
exp n (Succ m) = mul (exp n m) n -- n ^ (m + 1) = (n ^ m) * n
