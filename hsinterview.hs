
:{
let reverse [] y = y
    reverse (x:xs) e = reverse xs (x:e)
:}

reverse [1..3] []

:{
let rev2 [] = []
    rev2 (x:xs) = rev2 xs ++ [x]
:}

rev2 [1..10]


:{
let r3 [] = []
    r3 xs = foldl (swap (:)) [] xs
:}

r3 [1..10]

:{
let flt :: (a -> Bool) -> [a] -> [a]
    flt _ [] = []
    flt f (x:xs)
      | f x  = x : (flt f xs)
      | otherwise = flt f xs
:}


:t foldl

flip (:) [2,3] 3



:{
let map'            :: (a -> b) -> [a] -> [b]
    map' f []       = []
    map' f (x:xs)   = foldr (\y ys -> (f y):ys) [] xs
:}

map' (+1) [3,4]



let filt f xs = foldl (\x -> f x) [] xs

filt (<5) [1..10]

(<3) 4

flt (<3) [1..5]


let swap f a b = f b a

:t swap

swap [3:4]

3 : [4]

flip (:) [1] 2

flip (/) 3 4

:t flip

