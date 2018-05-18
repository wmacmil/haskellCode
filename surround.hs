:{
let insertAt :: a -> Int -> [a] -> [a]
    insertAt newElement _ [] = [newElement]
    insertAt newElement i (a:as)
      | i <= 1 = newElement:a:as
      | otherwise = a : insertAt newElement (i - 1) as
:}

insertAt 3 4 [1..10]

:{
let insertAtEnd a [] = [a]
    insertAtEnd a (x:xs) = x : insertAtEnd a xs
:}

insertAtEnd 'c' "asdf"

let surround x ys = x : insertAtEnd x ys

surround 'c' "asdf"


easierSurround x ys = x : ys ++ [x]
easierSurround 3 [1..4]

:{
let myConcat [] [] = []
    myConcat [] (y:ys) = y : myConcat [] ys
    myConcat (x:xs) (y:ys) = x : myConcat xs (y:ys)
:}

myConcat "asdf" "fdsa"


-- a surround with my own concatonation defined
let beforeAndAfter x y = myConcat x $ myConcat y x
beforeAndAfter "asdf" "fdsa"

-- e.g. belongsTo
:{
let questionMark x [] = False
    questionMark x (y:ys)
      | x == y = True
      | otherwise = questionMark x ys
:}
questionMark 3 [1..5] == True
questionMark 3 [4..5] == False


:{
let q2 xs [] = False
    q2 [] ys = True
    q2 (x:xs) (y:ys)
    -- wrong right here, need an extra condition if its not correct
      | x == y = q2 xs ys
      | otherwise = q2 (x:xs) ys
:}

-- matches if there is an a somewhere followed by a b anywhere else after it.  There's some regexp for this

q2 "ab" "aabc"
q2 "ab" "aacbc"
q2 "ab" "cbca"

questionMark 'a' "bc"
questionMark 'a' "bcaa"

:{
let q2 xs [] = False
    q2 [] ys = True
    q2 (x:xs) (y:ys)
    -- wrong right here, need an extra condition if its not correct
      | x == y = q2 xs ys
      | otherwise = q2 (x:xs) ys
:}



-- doesn't work, don't care to figure out why
-- :{
-- let q2 xs [] = False
--     q2 z@(x:xs) (y:ys)
--       | z == [] = True
--       | x == y = q2 xs ys
--       | otherwise = q2 z ys
-- :}








:{
data Tree a = Empty | Branch a (Tree a) (Tree a)
                  deriving (Show, Eq)
:}

let leaf x = Branch x Empty Empty

:{
let cbalTree :: Int -> [Tree Char]
    cbalTree 0 = [Empty]
    cbalTree n = let (q, r) = quotRem (n - 1) 2
        in [Branch 'x' left right | i     <- [q .. q + r],
                                    left  <- cbalTree i,
                                    right <- cbalTree (n - i - 1)]
:}

:{
let qR :: Int -> Int -> (Int, Int)
    qR x y = (quot0 x y, myMod x y)
:}

map (qR 100) [1..20]
:{
let quot' x y
  | (x >= y)   = quot' (x-y) y
  | otherwise = x
:}


:{
let myMod x y
      | (x < y) = x
      | otherwise = myMod (x - y) y
:}
-- x bigger, y smaller

myMod 55 11
myMod 55 13

map (myMod 55) [1..11]

-- quotient? Indepenent of
:{
let quot x y n
      | x < y = n
      | otherwise = quot (x-y) y (n+1)
:}

quot 18 6 0

quot x y = n
  where n
    | x > y = 1 + quot (x-y) y
    | otherwise = n


myQuot :: Int -> Int -> Int
myQuot x y = go x 0
  where
    go x n
       | x < y     = n
       | otherwise = go (x - y) (n + 1)



:{
let myQuot :: Int -> Int -> Int
    myQuot x y = go x 0
      where
        go x n
           | d < 0 = n
           | otherwise = go d (n + 1)
           where d = x - y
:}


:{
let myQuot :: Int -> Int -> Int
    myQuot x y = go x 0
      where
        go x n
           | d < 0 = n
           | otherwise = go d (n + 1)
           where d = x - y
:}

myQuot 100 3


quot :: Int a => a -> a -> a -> a

quot 100 3 0

let quot0 x y = quot x y 0

:{
let count :: (a -> Bool) -> [a] -> Int
    count _ [] = 0
    count p (x:xs)
       | p x       = 1 + count p xs
       | otherwise =     count p xs
:}


qt x y
  | x < y = 1 + qt (x-y) y
  | otherwise = 1 + qt (x-y) y

count (>3) [1..100]

l

myMod 100 3

rem 30 2

quot 100 3

:t qR

qR 10 2


map (qR 100) [1..10]

map (quotRem 100) [1..10]

:{
let pl3 :: Int -> Int
    pl3 x = x + 3
:}


:{
let cbalTree 0 = [Empty]
    cbalTree 1 = [leaf 'x']
    cbalTree n = if n `mod` 2 == 1 then
                 [ Branch 'x' l r | l <- cbalTree ((n - 1) `div` 2),
                                    r <- cbalTree ((n - 1) `div` 2) ]
                 else
                 concat [ [Branch 'x' l r, Branch 'x' r l] | l <- cbalTree ((n - 1) `div` 2),
                                                             r <- cbalTree (n `div` 2) ]
:}

cbalTree 5

:{
let freeTree :: Tree Char
    freeTree =
        Node 'P'
            (Node 'O'
                (Node 'L'
                    (Node 'N' Empty Empty)
                    (Node 'T' Empty Empty)
                )
                (Node 'Y'
                    (Node 'S' Empty Empty)
                    (Node 'A' Empty Empty)
                )
            )
            (Node 'L'
                (Node 'W'
                    (Node 'C' Empty Empty)
                    (Node 'R' Empty Empty)
                )
                (Node 'A'
                    (Node 'A' Empty Empty)
                    (Node 'C' Empty Empty)
                )
            )
:}

freeTree


mirror x y =

:{
let mirror Empty          Empty          = True
    mirror (Branch _ a b) (Branch _ x y) = mirror a y && mirror b x
    mirror _              _              = False
:}

:t mirror


symmetric x =


:{
data Tree a = Empty | Branch a (Tree a) (Tree a)
                  deriving (Show, Eq)
:}

let leaf x = Branch x Empty Empty

symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))





    myQuot :: Int -> Int -> Int
    myQuot x y = go x 0
      where
        go x n
           | d < 0 = n
           | otherwise = go d (n + 1)
           where d = x - y

