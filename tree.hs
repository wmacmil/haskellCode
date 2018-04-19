I'm trying to define fmap over a Tree type from hammar's answer in https://stackoverflow.com/questions/7624774/haskell-map-for-trees

His definition derives functor, which uses a pragma, which I'm only vaguely familiar with.  His definition is

    {-# LANGUAGE DeriveFunctor #-}
    data Tree a = Leaf a | Node (Tree a) (Tree a)
        deriving (Functor, Show)

I can't get the pragma and definition to work in GHCI.  Below are my three mistaken attempts, and I would appreciate any feedback!

First attempt:

    Prelude> {-# LANGUAGE DeriveFunctor #-}
    Prelude> data Tree a = Leaf a | Node (Tree a) (Tree a)
    Prelude>     deriving (Functor, Show)
    <interactive>:30:5: parse error on input ‘deriving’

Second try:

    Prelude> {-# LANGUAGE DeriveFunctor #-}
    Prelude> data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Functor, Show)
    <interactive>:32:57:
        Can't make a derived instance of ‘Functor Tree’:
      You need DeriveFunctor to derive an instance for this class
    In the data declaration for ‘Tree’

Third Try:

    Prelude> :{
    Prelude| {-# LANGUAGE DeriveFunctor #-}
    Prelude| data Tree a = Leaf a | Node (Tree a) (Tree a)
    Prelude|     deriving (Functor, Show)
    Prelude| :}
    <interactive>:35:1: parse error on input ‘data’




:{
data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Functor, Show)
:}

fmap (+1) (Node (Leaf 3) (Leaf 4))

{-# LANGUAGE DeriveFunctor #-}
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Functor, Show)


{-# LANGUAGE DeriveFunctor #-}
data Ree a = Leaf a | a Node (Ree a) (Ree a) deriving (Functor, Show)


data Graph a =


:{
data Graph a = Graph [a] [(a, a)]
               deriving (Show, Eq,Functor)
:}

let g = Graph [1,2,3] [(1,2),(1,3)]

:t g

-- is there any way to do this without explicitly referncing the graph in the input

let nodes (Graph nodes verts) = nodes

let nodes (Graph ns edges) = ns
let edges (Graph ns es) = es


nodes g
edges g

-- seems to work
let pathsFromN n g = fn n $ edges g

pathsFromN 3 g

pathsFromN 4 g

let g = Graph [1,2,3,4] [(1,2),(1,3),(2,3),(3,4)]

:t g

-- retern a list of lists with each list representing a unique path from n
--

:t [[3]]

highOrderPathsFromN :: Int -> Graph Int -> [[Int]]

[3] : [3] : []

(3,4) : (3,3) : []
(3,4,3) : [(3,4,3)]

let mapConsList x a@(y:ys) = map (\z -> x : z : []) a

mapConsList [3] [[3],[4]]

mapConsList 3 [3,4]
mapConsList (3,2) [(3,1),(4,4)]

mapConsList [(3,2)] [[(3,1)],[(4,4)]]

pathsFromN 1 g

let g = Graph [1,2,3,4] [(1,2),(1,3),(2,3),(2,4),(3,4)]

let e = edges g

e

-- define it for just an edge set, not a graph

let pathsFromNe n edges = fn n $ edges

pathsFromNe 1 e

scndO :: Int a => [(a,a)]
scndO

-- the e here has to come from the nodes out of 1

let mapSnd e = map snd e

mapSnd $ pathsFromNe 1 e

let nodes1 = mapSnd e

nodes1

let mapPaths ns e = map (\x -> pathsFromNe x e) ns

e

mapPaths (mapSnd $ pathsFromNe 1 e) e
pathsFromNe 1 e

-- [(1,2),(1,3)]

-- not quite, but almost, need to modify mapcons
--perhaps a zipwith mapcons? YES
--this should be our second order solution.  I'll clean it up tomorrow


zipWith mapConsList (pathsFromNe 1 e) (mapPaths (mapSnd $ pathsFromNe 1 e) e)

mapConsList 3 [3,4]
mapConsList (3,2) [(3,1),(4,4)]

let aaa = zipWith mapConsList (pathsFromNe 1 e) (mapPaths (mapSnd $ pathsFromNe 1 e) e)

:t aaa

mapPaths [2,3] e

mapPaths e nodes1


secondOrderPathsFromN n g =

highOrderPathsFromN n g =

let pathsToN n g = tn n $ edges g

pathsToN 3 g

paths n1 n2 g =

fst (1,2)
snd (1,2)

-- tn === toN

:{
let tn n [] = []
    tn n (x:xs)
      | n == snd x = x : tn n xs
      | otherwise = tn n xs
:}

tn 3 [(1,3),(3,1),(3,2)]

-- try to extract paths with same first node

:{
let fn n [] = []
    fn n (x:xs)
      | n == fst x = x : fn n xs
      | otherwise = fn n xs
:}

fn 3 [(1,3),(3,1),(3,2)]

-- alternatively, we can more easily use a filter


let filterFstIsN n y@(x:xs) = filter (\a -> n == fst a) y

filterFstIsN 3 [(1,3),(3,1),(3,2)]

fst (1,2)
snd (1,2)




fmap (+1) g

:{
data Adjacency a = Adj [(a, [a])]
                   deriving (Show, Eq,Functor)
:}

let adj1 = Adj [(3,[1,2]),(1,[3]),(2,[3])]

fmap (*11) adj1

:t adj1

let adj2 = [(3,[1,2]),(1,[3]),(2,[3])]

fmap (+1) adj2

