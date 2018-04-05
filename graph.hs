
:{
data Graph a = Graph [a] [(a, a)]
                   deriving (Show, Eq)
:}

data Mesh a = Mesh [a] [(a, a)] [(a, a, a)]

let a = Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]

let aa = Mesh ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')] [('b','f','c')]

let b = Graph [(0,1),(2,0)] [((0,1),(2,0))]

graphToAdj a

b

graphToAdj Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]

Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]


Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]


:{
data Adjacency a = Adj [(a, [a])]
                     deriving (Show, Eq)
:}

:{
data Friendly a = Edge [(a, a)]
                      deriving (Show, Eq)
:}

:{
let graphToAdj :: (Eq a) => Graph a -> Adjacency a
    graphToAdj (Graph [] _)      = Adj []
    graphToAdj (Graph (x:xs) ys) = Adj ((x, ys >>= f) : zs)
       where
          f (a, b)
             | a == x = [b]
             | b == x = [a]
             | otherwise = []
          Adj zs = graphToAdj (Graph xs ys)
:}

:{
let adjToGraph :: (Eq a) => Adjacency a -> Graph a
    adjToGraph (Adj [])          = Graph [] []
    adjToGraph (Adj ((v, a):vs)) = Graph (v : xs) ((a >>= f) ++ ys)
       where
          f x = if (v, x) `elem` ys || (x, v) `elem` ys
                then []
                else [(v, x)]
          Graph xs ys = adjToGraph (Adj vs)
:}



graphToFri :: (Eq a) => Graph a -> Friendly a
graphToFri (Graph [] _)  = Edge []
graphToFri (Graph xs ys) = Edge (ys ++ zip g g)
   where
      g = filter (\x -> all (\(a, b) -> x /= a && x /= b) ys) xs


friToGraph :: (Eq a) => Friendly a -> Graph a
friToGraph (Edge []) = Graph [] []
friToGraph (Edge vs) = Graph xs ys
   where
      xs = foldr acc [] $ concat $ map (\(a, b) -> [a, b]) vs
      ys = filter (uncurry (/=)) vs
      acc x xs = if x `elem` xs then xs else x : xs

adjToFri :: (Eq a) => Adjacency a -> Friendly a
adjToFri = graphToFri . adjToGraph

friToAdj :: (Eq a) => Friendly a -> Adjacency a
friToAdj = graphToAdj . friToGraph


