
-- we'll just define a continuous set of points by the bounding pts
-- i.e. the set [0,1] will similairly be denoted [0,1]
-- to construct the cantor set there are two basic functions: divide the current space in to three spaces, and make each space one third the size of the orignal space
-- [0,1] -> [[0,(1/3)],[(1/3),(2/3)],[(2/3),1]]
-- -> [[0,(1/9)],[(2/9),(1/3)],[(2/3),(7/9)],[8/9,9]]
-- the real data structure we want to generate is a tree, but a flattened list will be easier to visualize
-- 1/3,1/9,
--0,1
--0,1,2,3
--0,1,2,3,6,7,8,9
--0,1,2,3,6,7,8,9,18?
--so we are squaring the number of

1. Divide by 3
2. Concat (1) * 2


let divby3 (x:xs) = map (*(1/3)) (x:xs)

let a = divby3 [0,1]

map (+(1/3)) [0,1]

let y = fromIntegral 1 / 3 :: Rational
y

:{
let z :: Rational
    z = fromIntegral 4 / 9
:}

:{
let z2 :: Rational
    z2 = fromIntegral 4 / 10
:}

z2 + z + y

(1/38) + z

:{
let e :: Rational
    e = 3 / 4
:}

e

let f = fromIntegral 3 / 4

f

-- do it preseving the rational form
let yy n = fromIntegral 1 / (3^^n) :: Rational

yy 2

map (+y) [0,1]

map (\n -> (map (+(n/3)) [0,(1/3)])) [0,1,2]

let erkj p = map (\n -> (map (+(n/9)) p )) [0,2]

-- erkj takes the interval and maps it to
let erkj p a = map (\n -> (map (+(n/(3^^a))) p )) [0,2]

erkj [0,(1/3)] 1
erkj [0,(1/9)] 2

-- l === [0,1]

:{
let tr a 0 l = []
    tr a n l = map (map (tr (a+1) (n-1))) (erkj p a)
      where p = divby3 l
:}

let x = divby3 [0,1] in erkj x 1



-- this gets the correct second value
map (map (+ (2/3))) (erkj [0,(1/9)] 9)

map (map (+ (2/3))) (erkj [0,(1/27)] 27)

(erkj [0,(1/27)] 27)

1/27

erkj [0,(1/9)] 9

map (+(2/3)) (erkj [0,(1/9)] 9)

map (+ (2/3)) (head $ tail $ erkj [0,(1/9)] 9)

erjk [

:{
let asdf x m = erkj y (3^^m)
      where y = divby3 x
:}

asdf [0,1] 2

let y = fromIntegral 1 / 3 :: Rational

y

erkj [0,(1/3)] 3

erkj [(2/3),1] 3

erkj [(2/3),1] 9

asdf [(2/3),1] 2

asdf [0,(1/3)]

divby3 [0,1]

erkj [0,1/3] 3

erkj [0,(1/9)]

erkj [(2/3),1]





-- not quite
let partitionSet x = map (\n -> (map (+(n/x)) [0,(1/x)])) [0,2]

partitionSet 3

partitionSet 9


map (\n -> (map (+(n/9)) [0,(1/9)])) [0,2]

((\n -> map (+(n/3))) [0.1,2]) 3

map (+

divInto3

map (*(1/3)) [0,1]


-- wrong
-- let peoir p y = map (\n -> erkj n y) p
-- peoir [[0,(1/3)],[(2/3),1]] 3





:{
let union1 x [] = [x]
    union1 x y'@(y:ys)
      | x == y = y'
      | otherwise = y : union1 x ys
:}

union1 3 [1..4]

:{
let isElem x [] = False
    isElem x y'@(y:ys)
      | x == y = True
      | otherwise = isElem x ys
:}
isElem 5 [1..4]

:{
let isElemVal x y
      | isElem x y = []
      | otherwise = [x]
:}
isElemVal 34 [1..5]

:{
let union x'@(x:xs) y'@(y:ys) = foldl (++) [] (iev ++ [y'])
      where iev = map (\v -> isElemVal v y') x'
:}

union [3..5] [1,4,6]
union ['a'..'d'] ['z']
union [[1..5],[1..3]] [[1,3],[1..5]]

union [5,1,4] [3,4,5]

-- finally, geez.  union and concat are interchangeable, presumably the latter uses less memory

:{
let cantor 0 = [0,1]
    cantor n = union cnm1o3 (map (+(2/3)) cnm1o3)
      where cnm1o3 = map (*(1/3))  cnm1
            cnm1 = cantor (n-1)
:}

:t cantor

:{
let cantor :: Int -> [Rational]
    cantor 0 = [0,1]
    cantor n = union cnm1o3 (map (+(2/3)) cnm1o3)
      where cnm1o3 = map (*(1/3))  cnm1
            cnm1 = cantor (n-1)
:}

let op = cantor 3
:t op

let po = map fromRational op

op

map toRational op

let basdf = ((1/3) + (40/59))

:{
let basdf :: Rational
    basdf = ((1/3) + (40/59))
:}

basdf

toRational basdf

:{
let b :: Rational -> Fractional
    b a = (1/2) + a
:}

toFractional (3/2)

:t toRational

fromRational (3 % 2)

:t (3/2)

:t (2 % 3)

toRational (3/2)

b

:t (1/2)

cantor 3



:{
let cantor2 0 = [0,1]
    cantor2 n = (++) cnm1o3 (map (+(2/3)) cnm1o3)
      where cnm1o3 = map (*(1/3))  cnm1
            cnm1 = cantor2 (n-1)
:}


cantor2 0
cantor2 3

let c2 = cantor 2

cantor 5

c2

2*(1/3)
4*(1/9)

let o3toPn n = (2^n) / (3^n)

-- the cutoff point
o3toPn 646

let oneOn n = 1/n

oneOn (3^10090)

3^100903324




-- all nice looking, but irrelevant in this case
:{
let pairList [] = []
    pairList (x:y:xs) = pairs x y : pairList xs
      where pairs x y = [x,y]
:}

let pc2 = pairList c2
sumPairList pc2

:{
let sumPairList xs = map binDiff xs
      where binDiff [x,y] = y - x
:}



cantor 3

-- nub deletes redundant elements
:t nub
nub [1,4,3,2,34]

:t delete
delete 3 [1..5]

-- inadvertently defines intersection?
let union x'@(x:xs) y'@(y:ys) = map (\v -> isElemVal v y') x'

union [3..5] [1..10]

let zz = union [3..5] [1..10]
zz ++ [[3]]


-- original definiton
-- :{
-- let isElemVal x y
--       | isElem x y = [x]
--       | otherwise = []
-- :}



:t flip
flip isElem [1..10] 1

map (flip isElem [1..4]) [1..10]

map (\s -> union1 s [1..4]) [1..10]

let un x'@(x:xs) y'@(y:ys) = map (\v -> union1 v y') x'

:t un

un [1..5] [5..10]


