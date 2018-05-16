

longestCommonSubsequence

idea:
-- for each element in the left list, determine the longest common subsequence of sequences beginning with x (dependent on position)

lcs (x:xs) (y:ys) =


:{
let belongs x [] = False
    belongs x (y:ys)
      | x == y = True
      | otherwise = belongs x ys
:}

belongs 3 [1..5]
belongs 3 $ reverse [1..5]

drop 3 [1..5]

:{
let belongsAt x [] n = (-1)
    belongsAt x (y:ys) n
      | x == y = n
      | otherwise = belongsAt x ys (n+1)
:}

:t belongsAt

belongsAt 3 [1..5] 0

drop 2 [1..5]

subseq (x:xs) y'(y:ys)
  | belongs x y' = equalQ xs (belongsAt y')



let longest xs ys = if length xs > length ys then xs else ys

longest [1..51] [2..53]



:{
let lcs [] _ = []
    lcs _ [] = []
    lcs (x:xs) (y:ys)
      | x == y    = x : lcs xs ys
      | otherwise = longest (lcs (x:xs) ys) (lcs xs (y:ys))
:}


lcs [1,3] [1,2,1,3,4]

lcs [1..10] [1,3,4,5,6,7,8,32,34,32,1,4,2]

lcs asdf [1..20]

asdf

let asdf = ([1,2] ++ [15..30] ++ [3,2])

import Data.Array

:{
let lcs xs ys = a!(0,0) where
      n = length xs
      m = length ys
      a = array ((0,0),(n,m)) $ l1 ++ l2 ++ l3
      l1 = [((i,m),[]) | i <- [0..n]]
      l2 = [((n,j),[]) | j <- [0..m]]
      l3 = [((i,j), f x y i j) | (x,i) <- zip xs [0..], (y,j) <- zip ys [0..]]
      f x y i j
        | x == y    = x : a!(i+1,j+1)
        | otherwise = longest (a!(i,j+1)) (a!(i+1,j))
:}

let b x = array (3,4) x

zip [1..10] [2..20]

:t b


let a = array (1,100) ((1,1) : [(i, i * a!(i-1)) | i <- [2..100]])

length a

:t array

:t (3,2)




