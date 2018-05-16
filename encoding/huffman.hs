
:{
let countCs c [] = 0
    countCs c (x:xs)
      | c == x = 1 + countCs c xs
      | otherwise = countCs c xs
:}

:{
let quickSort [] = []
    quickSort (x:xs) = (quickSort left) ++ [x] ++ (quickSort right)
      where left = filter (<x) xs
            right = filter (>x) xs
:}

quickSort [2,3,454,5,45,6,54324,21,52,65,1]

:{
let sortBy f [] = []
    sortBy f (x:xs) = (sortBy f left) ++ [x] ++ (sortBy f right)
      where left = filterBy f (<=(f x)) xs
            right = filterBy f (>(f x)) xs
:}

b

sortBy snd b

-- here's the pointFree solution to construct the character frequency list
let step1 = sortBy snd . cA

let jf = step1 "ajsd jbjajdsj j jasdjfj jajfjj jajfj jksjkgjjaj"

:t jf

jf

(3,4) + (4,5)

'c' : ['d']

let stringChar a = ([fst a],snd a)

let mjf = map stringChar jf

head mjf
head $ tail mjf

-- only works, e.g. is closed, if the letters are one element strings
addArray (head mjf) (head $ tail mjf)

let addArray a b = (fst a ++ fst b,snd a + snd b)


addArray ("g",1) ("eb",0)
addArray ("g",1) ("",0)
foldr addArray ([],0) mjf

map Leaf mjf
-- hence a list of leafs/trees

-- inverse of map leaf
let leafVal (Leaf x) = x

:{
let treeVal (Leaf x) = x
    treeVal (Node x _ _) = x
:}

-- identity, or isomorphism
map leafVal (map Leaf mjf)

:t mjf

addFirsttwoTrees (x:xs) = addArray (head


treeVal $ t1 jf

t1 jf

leafVal $ head mjf


let realInput xs = map Leaf $ map stringChar xs

-- so after inserting this back in, sortby treeVal $ snd?
-- third try

t1 $ realInput jf

t2 $ realInput jf

let tr2 = t2 $ realInput jf

-- works, just need to generalize the recursion
t1 $ t2 $ realInput jf

realInput jf

t2 $ t2 $ t2 $ realInput jf

-- cool, works up to wikipedia!

t2 $ t2 $ t2 $ t2 $ t2 $ realInput $ step1 "A DEAD DAD CEDED A BAD BABE A BEADED ABACA BED"

t3 $ realInput $ step1 "A DEAD DAD CEDED A BAD BABE A BEADED ABACA BED"

t3 $ realInput $ step1 "this is an example of a huffman tree"

:t t3


:{
let t3 [x] = [x]
    t3 xs = t3 $ t2 xs
:}

let t2 (x:y:xs) = sortBy (snd . treeVal) $ ((t1 (x:y:xs)) : xs)

let t1 (x:y:xs) = Node (addArray (treeVal x) (treeVal y)) x y

:{
let t1 xs = Node (addArray (leafVal l1) (leafVal l2)) l1 l2
      where l1 = head mjf
            l2 = head $ tail mjf
            mjf = map Leaf $ map stringChar xs
:}


:{
let t1 = Node (addArray l1 l2) (Leaf l1) (Leaf l1)
      where l1 = head mjf
            l2 = head $ tail mjf
            mjf = map stringChar jf
:}

t1

let t2 = Node 3 (Leaf 3) (Leaf 4)


-- now, how to create the tree?

data Btree a = Leaf a | Node a (Btree a) (Btree a) deriving (Show)




--filterBy if we want to apply a simple filter to a more complex data structure, we can project it using f and then filter with g

:t filterBy

:{
let filterBy f g [] = []
    filterBy f g (x:xs)
      | g $ f $ x = x : filterBy f g xs
      | otherwise = filterBy f g xs
:}

filterBy snd (< 5) b

let b = countAll [1,23,2,1,1,4,1,12,2,4,21,12,21,1,21,3,3,2,5,25,5,2,2,3] []

let a = countAll "asjdjdjfsajajdsjfsjjzjajjajajsjdfjvfja" []

filter (<3) [1..5]
:t filter

:{
let filter f [] = []
    filter f (x:xs)
      | f x = x : filter f xs
      | otherwise = filter f xs
:}


-- sort a more general data structure?


countAll "asjdjdjfsajajdsjfsjjzjajjajajsjdfjvfja" []

cA "asjdjdjfsajajdsjfsjjzjajjajajsjdfjvfja"

let cA xs = countAll xs []

:{
let countAll [] e = e
    countAll (x:xs) e = countAll xs (belongsToAndAdd x e)
:}

:{
let belongsToAndAdd x [] = [(x,1)]
    belongsToAndAdd x (e:es)
      | x == (fst e) = (x,(snd e + 1)) : es
      | otherwise = e : belongsToAndAdd x es
:}

belongsToAndAdd 3 [(3,4),(4,5)]
belongsToAndAdd 3 [(3,4),(4,5)]
belongsToAndAdd 5 [(3,4),(4,5)]


  (x,(n+1)) : e
