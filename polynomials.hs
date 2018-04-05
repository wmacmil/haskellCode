-- x^n + a_n-1 x^n-1 + ... + a_1 x + a_0
-- evaluating this could be done as
-- a_0 + x * (a_1 + x * (a_2 + x * (.....)
-- horners rule

:{
let hr [] n = 0
    hr (x:xs) n = x + n * hr xs n
:}

hr [2,3,1] 3

:{
let polyAdd [] [] = []
    polyAdd (x:xs) [] = (x:xs)
    polyAdd [] (y:ys) = (y:ys)
    polyAdd (x:xs) (y:ys) = x + y : polyAdd xs ys
:}

polyAdd [0..4] [4..8]

polyAdd [0] [4..8]

polyAdd [1..4] [4..13]
polyAdd [4..13] [1..4]

foldl (+) 0 [1..4]

let nPolyAdd = foldl polyAdd [0]

nPolyAdd [[1],[1..4],[5..12],[3,6]]

let cMult c ys = map (*c) ys

cMult 3 [1..4]

let monomMult c ys = 0 : cMult c ys

monomMult 3 [1..4]

:{
let nMult 0 ys = ys
    nMult n ys = 0 : nMult (n-1) ys
:}

nMult 5 [1..4]

let cNMult c n z@(y:ys) = cMult c $ nMult n z

cNMult 3 4 [1..4]

-- is this a map . map somehow
-- try it with just cmult

:{
let pM _ [] _ = []
    pM n (x:xs) z@(y:ys) = cNMult x n z : pM (n+1) xs z
:}

let erty = pM 0 [5,4,0,1] [1,3,1]

-- this now matches wolframm alpha
nPolyAdd erty

polyMult (x:xs) (y:ys) = polyAdd

-- how to define multivariable polynomials?


