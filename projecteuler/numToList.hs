
let nTL x = """ ++ x ++ """


-- 348 = 3 * 100 + 4 * 10 + 8 * 1
--

:{
let nts 0 = 0
    nts x = 1 + nts (x-1)
:}

:{
let nts1 0 n = n
    nts1 x n = nts1 (x-10) (n+1)
:}

:{
let nts1 x n
      | x > 9 = nts1 (x-10) (n+1)
      | otherwise = n
:}

nts1 39 0

map (\x -> nts1 x 0) [10,20..120]

-- tm === tens multiple
-- natural to string
-- correct broken into three steps

-- highestdDigit

:{
let nts3 x n m
      | x > (m-1) = nts3 (x-m) (n+1) m
      | otherwise = n
:}

nts3 39 0 10

let ntsd x n m = x - (nts3 x n m) * m

ntsd 393 0 100

-- this is equivalent to show

:{
let nts4 0 n m = []
    nts4 x n m = nts3 x n m : nts4 (ntsd x n m) n (quot m 10)
:}

nts4 393 0 100
nts4 3939393939393939393934 0 1000000000000000000000

read "38" :: Int


:{
let abc :: Int -> [Char]
    abc x = "x"
:}

show 39

abc 38

quot 100 10

:{
let nts2 x [n] tm = n
    nts2 x q@(n:ns) tm
      | x > (tm-1) = nts2 (x-tm) (addoneToHead q)  tm
      | otherwise = n : nts2 x ns (quot tm 10)
:}

let addoneToHead (x:xs) = x+1 : xs

tail [1..4]

(3+1):4:[]

4 : 4

-- maybe return something like ([3],902)

nts2 3902 0 1000

show 39

number 39 "asdf"

let msh x = "x"
msh 3999999999

map (nts2 3902 0) [1000,100,10,1]


nts1 90 0

nTL 38
