
filter (<3) [1..5]

-- I had errored in not wrapping the x in []
-- also seems to work with

:{
let quicks [] = []
    quicks (x:xs) = quicks left ++ [x] ++ quicks right
      where left = filter (<x) xs
            right = filter (>x) xs
:}

:t quicks

quicks [1,5,3,2]

head [[1,5,3,2],[1,5,3,2]]

let hLaGoE c x a b = head $ lessAndGreater c x a b
let thLaGoE c x a b = head $ tail $ lessAndGreater c x a b

thLaGoE 3 [9,4,1,5,3,2] [] []

lessAndGreater 3 [9,4,1,5,3,2] [] []

let hLGE c x = hLaGoE c x [] []
let thLGE c x = thLaGoE c x [] []

hLGE 3 [9,4,1,5,2]
thLGE 3 [9,4,1,5,2]


let asdf (x:xs) = hLGE x xs

asdf [3,2,4,1,5]

asdf [3,2]
asdf [3,2]

let fdsa (x:xs) = thLGE x xs

fdsa [3]

fdsa []

thLGE 3 [11,9,4,1,5,2]

-- bunch of mistakes were made, but I finally resolved it by just having the [x] in the ceinter of the qs alg.

-- a slightly optimized version of qs which halves the work done.  should try to test it


:{
let qs [] = []
    qs (x:xs) = (qs front) ++ [x] ++ (qs back)
      where front = hLGE x xs
            back  = thLGE x xs
:}

qs [6.3,9.1,4,1,5,3,2]

:{
let qs (x:xs) = (qs front) ++ (qs back)
      where front = hLGE x xs
            back  = thLGE x xs
:}


z 3 4

hLGE 3 [9,4,1,5,3,2]

hLaGoE 3 [9,4,1,5,3,2] [] []

head $ tail [[1,5,3,2],[1,5,3,2,4]]


-- really should read lessAndGreaterOrEqualTo

:{
let lessAndGreater c [] a b = [a,b]
    lessAndGreater c (x:xs) a b
      | c > x = lessAndGreater c xs (x:a) b
      | otherwise = lessAndGreater c xs a (x:b)
:}

lessAndGreater 3 [9,4,1,5,2] [] []

filter (<3) [1..5]

filter (<1) [2..5]

filter (<3) [4..5]

:{
let minList [] = x
    minList [x] = x
    minList (x:xs) = minList y
      where y = filter (<x) xs
:}

:{
let minList [x] = x
    minList (x:xs)
      | x < y = x
      | otherwise = y
      where y = minList xs
:}

Just 3

Just [1..45]

minList [111,3,4,2,3]

minList [1,3,2,-1]

[50..1]

[3*x | x <- [1..4]]

[(51-x) | x <- [1..50]]


let n21 n = [((n+1)-x) | x <- [1..(n)]]

n21 40

min

import System.Random

:{
let randomList :: Int -> IO([Int])
    randomList 0 = return []
    randomList n = do
      r  <- randomRIO (1,6)
      rs <- randomList (n-1)
      return (r:rs)
:}

let a = randomList 34

a

minList a

data Person = Person String String Int Float String String deriving (Show)

let mike = Person "asdf" "fjd" 3 3 "asdf" "fjd"

:{
let mike2 :: Person
    mike2 = Person "asdf" "fjd" 3 3.3 "asdf" "fjd"
:}

mike2

-- data is used to define a new type

data Nat = Z | S Nat deriving (Show)

data Ll a = Empty | Cons a (Ll a) deriving (Show)

:{
let ei :: Ll Int
    ei = Empty
:}

:{
let ei :: Ll Int
    ei = Cons 3 (Cons 3 Empty)
:}

let fd = Cons 3 Empty

let ke = Cons 3 fd


:{
let cdr :: Ll a -> Ll a
    cdr (Cons _ xs) = xs
:}

cdr (Cons 3 (Cons 2 Empty))

car $ cdr (Cons 3 (Cons 2 Empty))

let cadr = car . cdr

cadr (Cons 3 (Cons 2 Empty))

head $ tail [3,2]

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

-- have a data point at the node as well

data RTree a = Leaf a | Branch a (RTree a) (RTree a) deriving (Show)


{-# LANGUAGE DeriveFunctor #-}
data Ree a = Leaf a | Branch a (Ree a) (Ree a) deriving (Functor, Show)

:{
let lTree :: RTree [Int]
    lTree = Branch [1,2,3] (Leaf [1..4]) (Leaf [2..5])
:}

let lTree1 = Branch [5,10] lTree lTree

mapRTree (map (+1)) lTree1

:{
let tree1 :: Tree Int
    tree1 = Branch (Leaf 3) (Leaf 4)
:}

:{
let tree1 :: RTree Int
    tree1 = Branch 5 (Leaf 3) (Leaf 4)
:}

tree1

let tree2 = Branch 6 tree1 tree1

tree2

let tree2' = mapRTree (*3) tree2

let tree3 = Branch 19 tree2 tree2'

tree3

mapTree (*3) tree2

:{
let mapRTree f (Leaf a) = Leaf (f a)
    mapRTree f (Branch n t1 t2) = Branch (f n) (go t1) (go t2)
      where go = mapRTree f
:}

:{
let mapTree f (Leaf a) = Leaf (f a)
    mapTree f (Branch t1 t2) = Branch (go t1) (go t2)
      where go = mapTree f
:}



cdr Empty

:{
let car :: Ll a -> a
    car (Cons x _) = x
:}


let b = errorEmptyList "head"

errorWithoutStackTrace :: forall (r :: RuntimeRep). forall (a :: TYPE r).
                          [Char] -> a

:{
let errorWithoutStackTrace s =
      let ?callStack = freezeCallStack emptyCallStack
      in error s
:}

:{
let errorEmptyList :: String -> a
    errorEmptyList fun =
      errorWithoutStackTrace (prel_list_str ++ fun ++ ": empty list")
:}


:t errorWithoutStackTrace

:t error


:{
let prel_list_str :: String
    prel_list_str = "Prelude."
:}

head []


tail []

tail [s

:t Z

:t (S Z)

:{
let qwer :: Nat
    qwer = S Z
:}

:t (<)

data Ord

sdf = S Z

:{
let rqwer :: Nat
    rqwer = S (S Z)
:}

qwer
rqwer

natPlus qwer rqwer

natPlus qwer Z

natPlus Z Z

data Nat = Z | S Nat deriving (Show)

:{
let natPlus a Z = a
    natPlus Z a = a
    natPlus (S a) (S b) = natPlus a (S (S b))
:}

:t natPlus

:{
let natPlus Z a = a
    natPlus (S a) b = natPlus a (S b)
:}

:{
let natPlus2 a Z = a
    natPlus2 (S a) (S b) = natPlus2 (S (S a)) b
:}

natPlus (S Z) (S (S Z))

natPlus2 (S Z) (S (S Z))

natMult (S Z) (S (S Z))

natMult (S (S Z)) (S (S Z))

:{
let natMult Z a = Z
    natMult (S a) (S b) = natPlus (S b) (natMult a (S b))
:}

:{
let natMult :: Nat -> Nat -> Nat
    natMult Z a = Z
    natMult (S a) b = natPlus b (natMult a b)
:}

-- for whatever reason this is not commutative
-- finally correct

:{
let natMult :: Nat -> Nat -> Nat
    natMult Z a = Z
    natMult (S a) b = natPlus (natMult a b) b
:}

:{
let natMult :: Nat -> Nat -> Nat
    natMult Z a = Z
    natMult (S a) b = natPlus b (natMult a b)
:}

-- finally correct

:{
let natMult :: Nat -> Nat -> Nat
    natMult a Z = Z
    natMult a (S b) = natPlus (natMult a b) a
:}

:t natMult

natMult (S (S Z)) (S (S Z))

natMult (S (S (S Z))) (S (S Z))

natMult (S (S (S Z))) Z

natMult (S Z) (S (S (S Z)))

natMult (S (S (S Z))) (S Z)

natPlus (S (S (S Z))) (S Z)

natPlus (S Z) (S Z)


natPlus (S (S Z)) (S (S Z))

:{
let lt :: Nat -> Nat -> Bool
    lt Z     (S b) = True
    lt (S a) (S b) = lt a b
:}

:{
let isPos x
      | x > 0 = True
      | otherwise = False
:}

map isPos [-2,-1,0,1,4]

:t 3


gt a b
  | (a-b) > 0 = True
  | otherwise = False


:{
let min a b
      | a <= b = a
      | otherwise = b
:}

min (-44) 4.1

foldl (+) 0 [1..4]

foldl min 99 [1..4]

foldl min 99 [1..4]

-- so to define minL via a fold requires a priori knowledge of some kind of upper bound to the minimum

let minL = foldl min 999999

minL [1,3,4,(-23),390321]


:{
let nto1 0 = []
    nto1 n = n : nto1 (n-1)
:}

:t nto1

nto1 50

:{
let isLowerBound x [] = True
    isLowerBound x (y:ys)
      | x <= y = isLowerBound x ys
      | otherwise = False
:}

isLowerBound 1.1 [1..5]
isLowerBound 8 [1..5]

isMin 3 [1..5]
isMin 8 [1..5]

minList [1..5]

  | x < restof xs = x
  | otherwise = minList xs


slowSort (x:xs)
  | x < slowSort xs = x
  | otherwise


I was trying to define natural number multiplication in Haskell, and kept getting the error below (corresponds to the second natMult definition below).

    Prelude> natMult (S (S Z)) (S (S Z))
    *** Exception: <interactive>:(4,5)-(5,45): Non-exhaustive patterns in function natPlus

Here's my code:

    data Nat = Z | S Nat deriving (Show)

    natPlus :: Nat -> Nat -> Nat
    natPlus Z a = a
    natPlus (S a) (S b) = natPlus a (S (S b))

After a bit of tinkering, I realized this definition works fine, whereas the second one below is broken.  The only difference is the order of the input parameters for natPlus.

    -- works fine
    natMult :: Nat -> Nat -> Nat
    natMult Z a = Z
    natMult (S a) b = natPlus (natMult a b) b

    -- gives gives the error above
    natMult :: Nat -> Nat -> Nat
    natMult Z a = Z
    natMult (S a) b = natPlus b (natMult a b)

:{
let natPlus Z a = a
    natPlus (S a) b = natPlus a (S b)
:}
