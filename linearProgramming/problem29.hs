

-- a ^ b for all a b belonging to two sets

[x^2 | x <- [1..4]]

[x^b | x <- [2..5], b <- [2..5]]


-- rr == remove repeat

:{
let rr x xs = x : fxs
      where fxs = filter (/= x) xs

let rrl (x:xs) = rr x xs

rrl [3,1,2,3,4,3,2,5]


-- remove repeat whole list
:{
let rrwl [] = []
    rrwl (x:xs) = rrl (x:(rrwl xs))
:}

rrwl ([3..5] ++ [1..50])

rrwl [3,1,2,3,4,3,2,5]

-- correct solution
length $ rrwl [x^b | x <- [2..100], b <- [2..100]]

1000^1000

-- problem something else
sum [x^x | x <- [1..1000]]

9110846700




pn n = n * (3*n -1) /2

let listpn n = map pn [1..n]


let belongs y xs = filter (== y) xs

:{
let bT y xs
      | belongs y xs == [] = False
      | otherwise = True
:}

bT 3 [1..5]

let ip = (foldr (+) 0 .) . (zipWith (*))

let pairs x y = [[i,j] | i <- x, j <-y]

pairs [1,5] [1,3]

-- bT == belongsto True
:t bT

let pentagonalPairs n = pairs (listpn n) (listpn n)

let asdf f = map (foldr f 0)

let sumPairs = asdf (+)
let diffPairs = asdf (-)

-- problem 44
isPent p =

sumIsPent p1 p

fIsPent





Pentagonal numbers are generated by the formula, Pn=n(3n−1)/2. The first ten pentagonal numbers are:


1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...

It can be seen that P4 + P7 = 22 + 70 = 92 = P8. However, their difference, 70 − 22 = 48, is not pentagonal.

Find the pair of pentagonal numbers, Pj and Pk, for which their sum and difference are pentagonal and D = |Pk − Pj| is minimised; what is the value of D?


