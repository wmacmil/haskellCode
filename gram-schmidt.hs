
-- projection


let ip = (foldr (+) 0 .) . (zipWith (*))

ip [1,0,1] [1,1,2]

:{
let proj e a = map (*b) e
      where b = (ip e a)/(ip e e)
:}


:{
let proj e a = map (*b) e
      where b = (ip e a)/(ip e e)
:}

let ip = (foldr (+) 0 .) . (zipWith (*))
:{
let proj e a = map (*b) e
      where b = (ip e a)/(ip e e)
:}

proj [1,1] [2,1]


let u1 xs = head xs

:{
let u2 xs = zipWith (-) a2 pa2
      where a2 = kth xs 2
            pa2 = proj u1 a2
            u1 = head xs
:}


-- we should have it return a list of lists
-- e.g. we should just have it return the matrix

-- works for x2
--FINALLY!!!!

:{
let uks aks 1 = [head aks]
    uks aks n = uk : us
      where uk = subtractLists ak sumProjUs
            ak = kth aks n
            sumProjUs = addListOfLists zs projectedUs
            zs = kzeros $ length ak
            projectedUs = map (\x -> proj x ak) us
            us = uks aks (n-1)
:}

uks ex 3

:{
let normalize vec = map (*a) vec
      where a = (1 / norm vec)
:}
let norm vec = (ip vec vec) ** 0.5

normalize [-1,2,4,2]
norm [2,2]

-- Q
map normalize $ reverse $ uks ex 3

let u = reverse $ uks ex 3


let q = map normalize $ reverse $ uks ex 3


-- Cool, I got it
matMatMult (transpose q) ex

q

69/175
6 /35


matMatMult (transpose a) q

-- R
-- == (Q transpose) A
-- we're assuming the internal lists represent columns, not rows
-- need to typecheck so that row and column sizes match up

let matrixVecMult m v = map (ip v) (transpose m)

matrixVecMult [[2,1],[0,1],[3,2]] [1,1,1]

let matMatMult m1 m2 = map (matrixVecMult (transpose m1)) m2

-- I think this is right, based off the example below
let matMatMult m1 m2 = map (matrixVecMult m1) m2

matMatMult [[1,2],[3,4],[10,20]] [[0,1,1]]









:t kzeros

:{
let kth (x:xs) 1 = x
    kth (x:xs) k = kth xs (k-1)
:}

let ip = (foldr (+) 0 .) . (zipWith (*))

:{
let proj e a = map (*b) e
      where b = (ip e a)/(ip e e)
:}

-- there was something wrong with the prior type signature of kzeros in my somewhat degenerate definition

:{
let kzeros 0 = []
    kzeros k = 0 : kzeros (k-1)
:}
let subtractLists = zipWith (-)
let addListOfLists = foldr (zipWith (+))



-- correct
:{
let u2 xs = zipWith (-) a2 spa2
      where a2 = kth xs 2
            spa2 = foldr (zipWith (+)) [0,0,0] pa2
            pa2 = map (\x -> proj x a2) us
            us = [(u1 xs)]
:}

-- we should have it return a list of lists
-- e.g. we should just have it return the matrix
:{
let u3 xs = zipWith (-) a3 spa3
      where a3 = kth xs 3
            spa3 = foldr (zipWith (+)) [0,0,0] pa3
            pa3 = map (\x -> proj x a3) us
            us = [(u1 xs),(u2 xs)]
:}

firstK xs (n-1)

-- can we generalize this by iterating over a larger xs every time?


u1 ex
u2 ex
u3 ex

let ex = transpose example

ex

let example = [[12,-51,4],[6,167,-68],[-4,24,-41]]

import Data.List


map (\x -> proj x ak) (u:us)





-- ak = kth element of the input matrix

-- almost certain this doesn't work because we nowhere connstruct us? ***

-- the us has type [a] and the error comes from it expecting type [[a]]
-- how to resolve?
-- us needs to be alist of us!  look at ***
(map (\x -> proj x ak) us)


:{
let uk :: (Eq a, Num a) => [[a]] -> Int -> Int -> [a]
    uk as 0 n = head as
    uk as k n = zipWith (-) ak (foldr (zipWith (+)) kzs (map (\x -> proj x ak) us))
      where ak = kth as k
            us = uk (firstK as (k-1)) (k-1) n :
            -- something needs to be consed above
            kzs = kzeros n
:}

kth [1,2] 2



:t firstK

firstK [1..10] 3

firstK [[1],[2]] 1

-- obviously, not the best soln
let kzeros k = map (*0) [1..k]

kzeros 3

:{
let firstK _ 0 = []
    firstK (x:xs) k = x : firstK xs (k-1)
:}

:{
let kth (x:xs) 1 = x
    kth (x:xs) k = kth xs (k-1)
:}

kth [1..10] 5


uk [a] = a
uk as = zipWith (-) ak (foldr (zipWith (+)) (map (\x -> proj x ak) us))
  where ak = kth as k
        us = uk (firstK as)


foldr (zipWith (+)) (map (\x -> proj x ak) (u:us))
map (\x -> proj x ak) (u:us)

-- create an ongoing list of u's and then map the projection over it
--


-- doesn't terminate
foldl (+) 0 [1..]

-- stack overflow
foldr (+) 0 [1..]

un = foldl (map (-))

-- here we can see the flaw in the logic
(foldr (-) 0 $ reverse [1..5])  == (5-4+3-2+1-0)

foldr (zipWith (-)) [0,0] [[2,3],[1,1],[2,1]]

foldl (zipWith (-)) [0,0] [[2,3],[1,1],[2,1]]

fr (zipWith (-)) [0,0] [[2,3],[1,1],[2,1]]

fr (zipWith (+)) [0,0] [[2,3],[1,1],[2,1]]

-- a different type of fold
-- this is kind of like a foldl

:{
let fr f z [x] = f x z
    fr f z (x:y:xs) = fr f z ((f x y):xs)
:}

foldl f z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

-- ugg, whats wrong
-- below is defn of foldr

:{
let fold f z [] = z
    fold f z (x:xs) = f x (fold f z xs)
:}

zipWith (-) [2,3] (zipWith (-) [1,1] (zipWith (-) [2,1] [0,0]))

(zipWith (-) [2,1] [0,0])

(zipWith (-) [1,1] (zipWith (-) [2,1] [0,0]))

zipWith (-) (zipWith (-) [2,3] [1,1]) [2,1]

-- so right associativity switches the subtraction signs in the recursion
2 - (1 -2) == 2 - (-1) == 3
2 -1 + 2
vs
(2 - 1) - 2 == 1 - 2 == -1

zipWith (-) [2,3] (zipWith (-) [1,1] (zipWith (-) [2,1] [0,0]))

-- normalized vector


1 / ip [2,0] [2,0]

:{
let normalize vec = map (*a) vec
      where a = (1 / norm vec)
:}
let norm vec = (ip vec vec) ** 0.5

normalize [-1,2,4,2]
norm [2,2]

-- interesting that this comes out nice



:t n

n [1,0]
n [1,1]

n [2,0]

2 / (3 ** 0.5)

0.33333333333 ** 0.5

n e



