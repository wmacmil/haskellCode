
:{
let dupli [] = []
    dupli (x:xs) = x : x : dupli xs
:}

dupli [1..10]

-- 15
:{
let multi [] n = []
    multi (x:xs) n = nXs n x ++ multi xs n
:}

multi [1..10] 3

-- 16

:{
let dropModN [] _ _ = []
    dropModN (x:xs) n m
      | n == 1 = dropModN xs m m
      | otherwise = x : dropModN xs (n-1) m
:}

dropModN [1..20] 3 3


split2 xs 0 = xs
split2 (x:xs) n = x : split2 xs (n-1)


:{
let split2 ys n = (take ys n, leave ys n)
      where take _ 0 = []
            take (x:xs) n = x : take xs (n-1)
            leave xs 0 = xs
            leave (x:xs) n = leave xs (n-1)
:}

-- apparently the take pattern matching is noncommutative

:{
let take _ 0 = []
    take (x:xs) n = x : take xs (n-1)
:}

take [1..5] 3

drop 3 [1..5]

split2 [1..5] 3


let splice xs a b = take (b-a+1) $ drop (a-1) xs

splice ['a','b','c','d','e','f','g','h','i','k'] 3 7

take 3 [1..10]

splice [1..10] 3 6


let rotate xs n = drop n xs ++ take n xs

rotate [1..10] 3

let rmAt xs n = take (n-1) xs ++ drop n xs

rmAt "abcd" 2

removeAt xs n =




