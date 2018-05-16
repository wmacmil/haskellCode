
data ArrayTree a = Empty | Branch a [ArrayTree a] deriving (Show, Eq)

:{
let t1 :: ArrayTree Int
    t1 = Branch 3 [Empty,Empty,Empty]
:}

let at1 = Branch 4 [t1,t1,t1]
let at2 = Branch 5 [t1,at1,at1,t1]




:{
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
:}
let t1 = Branch 3 Empty Empty
let t2 = Branch 4 t1 t1
let t3 = Branch 5 t1 Empty
let t4 = Branch 6 t3 t2

t2 == t2

isSym t2

let t5 = Branch 2398 t2 t2

isSym t5

isSym t3
isSym t4

flip t4

flipt t4

:t flip

:{
let flip :: Tree a -> Tree a
    flip (Empty) = Empty
    flip (Branch x a b) = Branch x b a
:}


:{
let flipt (Empty) = Empty
    flipt (Branch x a b) = Branch x (flipt b) (flipt a)
:}

:{
let isSym t
      | t == (flipt t) = True
      | otherwise = False
:}


data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show)

:{
let maptree f (Leaf a)= Leaf (f a)
    maptree f (Node x xl xr ) = Node (f x) (maptree f xl) (maptree f xr)
:}

fl (Leaf a) = Leaf a
fl


:t maptree

