
-- for polynomials
--

:{
let pAdd [] [] = []
    pAdd xs [] = xs
    pAdd [] ys = ys
    pAdd (x:xs) (y:ys) = x + y : pAdd xs ys
:}

pAdd [1..5] [1..10]

let multByC n z@(y:ys) = map (*n) z



pMult (x:xs) (y:ys) =

derivative f x =

let p x = x^2 - 3*x

let multbyY (x:xs) y = map (*y) (x:xs)

listToP (x:xs) y = x + listToP

p 4
