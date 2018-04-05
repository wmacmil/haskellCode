
-- think polnomial multiplication except with a intersection operation pointwise

:{
let intersection x [] = []
    intersection x (y:ys)
      | x == y = [x]
      | otherwise = intersection x ys
:}

intersection 3 [1..4]
intersection 3 [4..43]

:{
let intersections [] _ = []
    intersections xs'@(x:xs) ys'@(y:ys) = intersection x ys' ++ intersections xs ys'
:}

intersections [4,1,2,3,5,7,99] [1..4]

take 1 [1]




:{
let multOfm n m
      | mod n m == 0 = True
      | otherwise = False
:}

multOfm 10 3

map (\x -> multOfm x 3) [1..100]

let asdf n = map (\x -> multOfm n x)

asdf 1000 [5,3]

map (\x -> asdf x [3,5]) [1..1000]


mod 5 3 == 0

:{
let div3or5 n
      | (mod n 3 == 0) || (mod n 5 == 0) = True
      | otherwise = False
:}

sum $ filter div3or5 [1..999]

let problem_1  = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]




import Data.List (union)

let problem_1' = sum (union [3,6..999] [5,10..999])

problem_1'

map boolToBin asdf

:{
let boolToBin x
      | x == True = 1
      | otherwise = 0
:}

isDivisbyNs n (x:xs) =

filter (/= 3) [1..5]


3 /= 3

:{
let delRedundant (x:xs) = x : filtEqx x xs
      where filtEqx x y = filter (/= x) y
:}

:{
let dR1 (x:xs) = filtEqx x xs
      where filtEqx x y = filter (/= x) y
:}

-- the next three functions define myunion

let dR2 (x:xs) = filter (/= x) xs

:{
let ddd [] = []
    ddd (x:xs) = x : dR2 (x: ddd xs)
:}

-- compressed from above
:{
let dddd [] = []
    dddd (x:xs) = x : dR2 (x: dddd xs)
      where dR2 (x:xs) = filter (/= x) xs
:}

let myUnion xs ys = dddd $ xs ++ ys

let myUnion xs ys = ddd $ xs ++ ys

union [3,5,2,1] [3,2,3,2]

myUnion [3,5,2,1] [3,2,3,2]

myUnion [3,5,4,2,1] [3,2,3,8,2,8]
union [3,5,4,2,1] [3,2,3,8,2,8]

myUnion [1..25] [1,3..60]


ddd [3,5,4,2,1,3,2,3,8,2,8]
ddd [3,5,4,2,1,3,2,3,8,2,8]

ddd [3,5,2,1,3,2,3,2]


dR2 [3,5,2,1,3,2,3,2]

dR1 [3,5,2,1,3,2,3,2]

dd (x:xs) = delRedundant

let ddd (x:xs) = delRedundant x : delRedundant
xs

delRedundant [3,45,3,6,2,3]



union                   :: (Eq a) => [a] -> [a] -> [a]
union                   = unionBy (==)

unionBy                 :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys        =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

let union (x:xs) (y:ys) = map (\s ->  filter (/= s) (y:ys)) (x:xs)

let union (x:xs) (y:ys) = map (\s ->  filter (== s) (y:ys)) (x:xs)



union [1..10] [1,5,11,2]

union [3,6..999] [5,10..999]

let problem_1' = sum (union [3,6..999] [5,10..999])




map (

deleteRedundant [x] = [x]
deleteRedundant (x:xs)
  | x == head xs = deleteRedundant xs
  | otherwise = x : deleteRedundant xs



