1/x + 1/y = 1/n


1/4 = 1/2*3 + 1/2*2*3
    = 2/2*2*3 +1/2*2*3
    = 3/2*2*3

  = 1/5 + 1/2*2*5
  = 1/2*2*2 + 1/2*2*2


:{
let a :: Integer -> Rational
    a x = 1 / x
:}

import Data.Ratio


let a = 3 % 4 :: Rational
let b = 5 % 7 :: Rational

let c = 3 % 8

:{
let d :: Integer -> [Rational]
    d n = map (\x -> 1 % x) [1..n]
:}

let a = d 2000

let b = d 5

nmp b

:{
let asdf b@[x,y] c
      | (x + y) == c = b
      | otherwise = []
:}

asdf [1 % 5, 1 % 20] (1 % 4)

let diop n m = map (\x -> asdf x (1 % n)) (nmp $ d m)

does way too muc work, but I can't think of an altenative algorithm.  the one

map length $ map (\x -> filter (/= []) $ diop x 2000) [1..200]

let x = filter (/= []) $ diop 180180 100000


import Data.List

:{
let primes=[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73]
    series _ 1 =[[0]]
    series xs n =[x:ps|x<-xs,ps<-series [0..x] (n-1) ]
    distinct=product. map (+1)
    sumpri x=product $zipWith (^) primes x
    prob x y =minimum[(sumpri m ,m)|m<-series [1..3] x,(>y)$distinct$map (*2) m]
    problem_108=prob 7 2000
:}

everlasting series [1..3] 180180

problem_108

-- concatenates too much
-- foldr (++) [] $ diop 4 20



filter (== 1 % 30) $ map (foldr (+) 0) (pairsLists a a)

filter (== 2) [1,2,3,2,4]

let pairsLists xs ys = [[x,y] | x <- xs, y <- ys]

pairsLists [1..4] [1..4]

-- how to do it assuming the pairs are "commutative"

:{
let mappairs n [] = []
    mappairs n (x:xs) = [n,x] : mappairs n xs
:}
:{
let nmp [] = []
    nmp y@(x:xs) = mappairs x y ++ nmp xs
:}

mappairs 1 [1..4]
nmp [1..4]

length $ nmp [1..4]





:t map (+ (1/2)) d

a + b + c


-- so, given n, how to find x and y?
