span :: (a -> Bool) -> [a] -> ([a], [a])

:{
let span' f [] = ([],[])
    span' f (x:xs)
      | f x = let (y,z) = span' f xs in (x:y,z)
      | otherwise = ([],xs)
:}

span' (< 3) [1..23]

let bc = span' (< 3) [1..23]

:{
let sp f [] = ([],[])
    sp f (x:xs)
      -- recursive call sp f xs
      | f x =
        let (y,z) = sp f xs
        in (x:y,z)
      | otherwise = ([],xs)
:}

sp (<4) [1..10]

s (<4) [1..10]


3 < 4

:{
let s f [] = []
    s f (x:xs)
      | f x = x : s f xs
      | otherwise = []
:}

:{
let p f [] = []
    p f y@(x:xs)
      | f x = p f xs
      | otherwise = y
:}

p (<4) [1..10]

let sp f x = (s f x,p f x)

sp (<4) [1..10]



snd bc

let sndSpan f x = snd (span f x)

let sndSpan f x = snd $ span f x

snd $ span (<4) $ sndSpan (<3) [1..5]

span (sndSpan (== 3) [1,3,2,3,45])

span (sndSpan (== 3) [1,3,2,3,45])

recurSpan f @y(x:xs) = span span f y


span                    :: (a -> Bool) -> [a] -> ([a],[a])
span _ xs@[]            =  (xs, xs)
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)


span (< 3) [1,2,3,4,1,2,3,4]

:{
let grp [] = []
    grp (x:xs) = (x:(filter (==x) xs)):(grp $ filter (/=x) xs)
:}

grp [1,2,3,4,5,432,5,23,4,2,14,2343,2,12]

pack [1,2,3,4,5,432,5,23,4,2,14,2343,2,12]

:{
let pack (x:xs) = let (first,rest) = span (==x) xs
                   in (x:first) : pack rest
    pack [] = []
:}


