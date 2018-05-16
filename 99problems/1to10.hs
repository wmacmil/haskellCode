
last :: [a] -> a


:{
let last [x] = x
    last (x:xs) = last xs
:}

:t last
last [1..10]

:{
let scndLast [x,y] = x
    scndLast (x:xs) = scndLast xs
:}

scndLast [1..10]

:{
let kthElem 1 (x:xs) = x
    kthElem k (x:xs) = kthElem (k-1) xs
:}

kthElem 3 [1..10]

(!!) [1..10] 3


-- really should be Nat
length' :: [a] -> Int

:{
let length' [] = 0
    length' (x:xs) = 1 + length' xs
:}

:t length'
length' [1..10]

-- inefficient
:{
let rev [] = []
    rev (x:xs) = rev xs ++ [x]
:}

rev [1..10]


-- The reduntant x:xs in the below defn muddys the understanding

:{
let rev2 l = r l []
      where r [] b = b
            r (x:xs) b = r xs (x : b)
:}

-- defn was incorrect when I specified the structure of the linked list
-- e.g. replace l with (x:xs) above to get a nontotal fcn

rev2 []

:{
let reverse :: [a] -> [a]
    reverse list = reverse' list []
      where
        reverse' [] reversed     = reversed
        reverse' (x:xs) reversed = reverse' xs (x:reversed)
:}

reverse []

rev2 [1..10]

rev2 $ [[x,y] | x <- [1..10], y <- [1..20]]

-- isPalindrome
--
--reverse is not properly defined

:{
let ip x
      | x == rev2 x = True
      | otherwise = False
:}

ip "asdffdsa"

ip []

:{
let ip2 x
      | x == reverse x = True
      | otherwise = False
:}

ip2 []

let isPalindrome xs = xs == (reverse xs)

isPalindrome []

-- for completeness
:{
ip [] = True
ip [_] = True
ip x
  | x == rev2 x = True
  | otherwise = False
:}

ip []

ip [1,2,3,2,1]
ip "asdffdsa"
ip [[1,0],[2,0],[1,0]]

[3]

[[[3]],[[3]]]

data NestedList a = Elem a | List [NestedList a] deriving (Show,DeriveFunctor)

fmap (+3) a

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)


data Tree a = Leaf a | Branch a (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch x left right) = Branch (f x) (fmap f left) (fmap f right)


let t1 = Branch 3 (Leaf 4) (Leaf 4)

fmap (+3) t1

fmap (+3) t1


let a = List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]

flatten a

:t flatten

:{
let flatten (Elem a) = [a]
    flatten (List (x:xs)) = flatten x ++ flatten (List xs)
    flatten (List []) = []
:}




-- eliminate consecutive duplicates

:{
let eD [] = []
    eD [x] = [x]
    eD (x:xs)
      | x == (head xs) = eD xs
      | otherwise = x : eD xs
:}
eD [3,3,3,2,2,3,3,4,4,5,6,6,5]
eD []

:{
let local (x:xs) n
      | x == (head xs) = local xs (n+1)
      | otherwise = (nXs n x) : local xs 1
:}

local [

local [3,3,3,2,2,3,3,4,4,5,6,6,5,5,5] 1

[3] : [[3]]


-- finally, I've constructed the correct solution!

:{
let packConsecDuplicates (x:xs) = local (x:xs) 1
      where local [x] n = [nXs n x]
            local (x:xs) n
              | x == (head xs) = local xs (n+1)
              | otherwise = (nXs n x) : local xs 1
:}

nXs 3 4
nXs 3 []

packConsecDuplicates [3,3,3,2,2,3,3,4,4,5,6,6,5,5,5]


packConsecDuplicates ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a','a', 'd', 'e', 'e', 'e', 'e']

:{
let runLengthEncode ls = map pr $ packConsecDuplicates ls
      where pr y@(x:xs) = (length y,x)
:}

runLengthEncode "aaabbbccddeeee"

:{
let local [x] n = [nXs n x]
    local (x:xs) n
      | x == (head xs) = local xs (n+1)
      | otherwise = (nXs n x) : local xs 1
:}

:{
let nXs 0 x = []
    nXs n x = x : nXs (n-1) x
:}
nXs 3 4

pd :: [x] -> [[x]]
pd (x:xs)
  | x == (head xs) = [x] : pd xs
  | otherwise =




data L a = Empty | Cons a (L a) deriving (Show)

let aa = Cons 3 (Cons 4 (Empty))

let b = Cons 4 aa

:{
let mm :: (a -> b) -> L a -> L b
    mm f Empty = Empty
    mm f (Cons x xs) = Cons (f x) (mm f xs)
:}

mm (+3) b

:{
let nXs 0 x = []
    nXs n x = x : nXs (n-1) x
:}
nXs 3 4
