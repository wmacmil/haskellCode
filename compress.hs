-- > compress "aaaabccaadeeee"
-- "abcade"
--
--
--

compress (x:xs)
  | x == head xs = xs
  | otherwise = x : compress xs


-- try take 1

:{
let compress [] = []
    compress [x] = [x]
    compress (x:xs)
      | x == head xs = compress xs
      | otherwise = x : compress xs
:}

:{
let compress [x] = [x]
    compress (x:xs)
      | x == head xs = compress xs
      | otherwise = x : compress xs
:}


3 == (2+1) && 3 ==

compress "aaaabccaadeeeef"
compress "aaaabccaadeeee"

compress $ [1..5] ++ [5,1,1,1,3]

3 == []

head "asdf"

compress "asasdfasasdfsdfasdddkdddddddd"


-- pack consecutive duplicates

-- *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',
--              'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]


(3:4:[]) : (4:5:[]) : []

span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])

span (< 9) [1,2,3] == ([1,2,3],[])

span (< 0) [1,2,3] == ([],[1,2,3])


-- be really careful with the where syntax

:{
let sp (x:xs) = (a,b)
      where a = [x]
            b = xs
:}

span f (x:xs) = (a,b)
  where a | f x =


span                    :: (a -> Bool) -> [a] -> ([a],[a])
span _ xs@[]            =  (xs, xs)
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)


span                    :: (a -> Bool) -> [a] -> ([a],[a])
span _ xs@[]            =  (xs, xs)
span p xs@(x:xs')
         | p x          =  let (ys,zs) = span p xs' in (x:ys,zs)
         | otherwise    =  ([],xs)


:{
let span f [] = ([],[])
    span f y@(x:xs)
      | f x = let (ys,zs) = span f xs in (x:ys,zs)
      | otherwise = ([],xs)
:}

span (<=5) [1..10]


:{
let pack [] = []
    pack (x:xs) = let (first,rest) = span (==x) xs
                   in (x:first) : pack rest
:}

:{
let pack (x:xs) = let (first,rest) = span (==x) xs
                   in (x:first) : pack rest
    pack [] = []
:}

pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']

["aaaa","b","cc","aa","d","eeee"]


pack [1..45]

pack [1,1,1,3,3,3,4,5,5,5,2]

pack [1..45]

pack [2..45]


:{
let grp [] = []
    grp (x:xs) = (x:(filter (==x) xs)):(grp $ filter (/=x) xs)
:}

grp ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']

grp (




let (x,y) = 3 in (x,y)

filter (== 3) [1,3,3,2,3,4,5]

:{
let span1 f y@(x:xs) = (a,b)
      where a = fi f y
            b = fo f y
:}

:{
let fi f [] = []
    fi f (x:xs)
      | f x = x : fi f xs
      | otherwise = []
:}

:{
let fo f [] = []
    fo f (x:xs)
      | f x = fo f xs
      | otherwise = (x:xs)
:}

span1 (<4) [1..10]

span1 (<11) [1..10]

span f y@(x:xs) = (a,b)
  where a = fi y
        b = fo y

:{
let fi f [] = []
    fi f (x:xs)
      | f x = x : fi f xs
      | otherwise = []
:}

fi (<6) [1..5]

fo (<6) [1..5]

:{
let fo f [] = []
    fo f (x:xs)
      | f x = fo f xs
      | otherwise = (x:xs)
:}


:{
let fo f (x:xs)
      | f x = fo f xs
      | otherwise = (x:xs)
:}

pack (x:xs)
  | x == head xs = x : head xs


