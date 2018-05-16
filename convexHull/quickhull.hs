-- Input : Set of N pts
--
--

-- can do it if we define infinity, remember
--

head [1..5]

:{
let minMax f x y
      | f x y = x
      | otherwise = y
:}

let min = minMax (<)
let max = minMax (>)

:{
let minMaxL f g [x,y] = g x y
    minMaxL f g (x:xs)
      | f x (head xs) = minMaxL f g (x : tail xs)
      | otherwise = minMaxL f g xs
:}

minMaxL (<) min [4,3,45,3,44]
minMaxL (>) max [4,3,45,3,44]



head [1]

:{
let ltXC f xs ys
      | f (head xs) (head ys) = xs
      | otherwise = ys
:}

: ltXC

ltXC (<) [1,4] [2,3]

-- (1) in wikipedia
--

let infinity = read "Infinity"

let minXCoord = foldr (ltXC (<)) [infinity]
let maxXCoord = foldr (ltXC (>)) [-infinity]


maxXCoord [[1,4],[0.5,0.9],[2,3]]
minXCoord [[1,4],[0.5,0.9],[2,3]]

-- how to divide the set of points into two?
-- how about an aboveLine & belowLine algorithm

-- how to represent a line in haskell?
--
-- y = m x + b
-- b where x = 0
-- m = y2 - y1 / x2 - x1

let m [x0,y0] [x1,y1] = (y1 - y0) / (x1 - x0)
let b [x0,y0] m = y0 - (m*x0)

:{
let l x [x0,y0] [x1,y1] = (x * m) + b
      where m = (y1 - y0) / (x1 - x0)
            b = y0 - (m*x0)
:}

head $ tail [1,4]

l 1 [1,2] [-4,-2]

l 2 [0.5,0.9] [2,3]

:t l

-- some floating point error is causing my aboveBelow fcn to mess up


:{
let aboveBelow :: (Fractional x, Ord x) => (x -> x -> Bool) -> [x] -> [x] -> [x] -> [x]
    aboveBelow f xs ma mi
      | f xz eval = xs
      | otherwise = []
        where xz = head $ tail xs
              eval = l (head xs) ma mi
:}

let above = aboveBelow (>)
let below = aboveBelow (<)

max1

above max1 max1 min1
above min1 max1 min1


-- -- so it works without the type signature, e.g. the type signature above was messing me up
-- :{
-- let above xs ma mi
--       | xz > eval = xs
--       | otherwise = []
--         where xz = head $ tail xs
--               eval = l (head xs) ma mi
-- :}

:t above


l 1 [0.5,0.9] [2,3]

above [0.5,0.9] [0.5,0.9] [2,3]
below [2,3] [0.5,0.9] [2,3]
above [2,3] [0.5,0.9] [2,3]

-- :{
-- let above xs = map (\x -> l x max min)
--       where max = maxXCoord  xs
--             max = maxXCoord  xs
-- :}
-- -- above versus below the line

import System.Random
let newRand = randomIO :: IO Int

:{
let randomList :: Int -> [Double]
    randomList seed = randoms (mkStdGen seed) :: [Double]
:}

let l1 = take 20 $ randomList 30

let max1 = maxXCoord $ pairsL l1
let min1 = minXCoord $ pairsL l1

[min1,max1]

aboveL l1

let above2 x y z = above z x y

map (\x -> above x min1 max1) l1

:{
let aboveBelowL f l = map (\x -> f x min max) p
      where min = minXCoord p
            max = maxXCoord p
            p = pairsL l
:}

let aboveL = aboveBelowL above
let belowL = aboveBelowL below

:t aboveL

max1

-- none of these indicated anything wrong
hasElem max1 (aboveL l1)
hasElem min1 (aboveL l1)
hasElem max1 (belowL l1)
hasElem min1 (belowL l1)

belowL l1

:{
let hasElem y [] = False
    hasElem y (x:xs)
      | y == x = True
      | otherwise = hasElem y xs
:}


let al = aboveL l2
let bl = belowL l2

let max2 = maxXCoord (pairsL l2)
let min2 = minXCoord (pairsL l2)

hasElem max2 al
hasElem max2 bl
hasElem min2 al
hasElem min2 bl

al
bl
max2
min2

map (map bi) [al,bl]

map (map bi) [al,bl]

foldr (zipWith (+)) [0,0,0,0,0,0,0,0,0,0]  $ map (bi) [al,bl]

:{
let bi [] = []
    bi (x:xs)
      | x == [] = 0 : bi xs
      | otherwise = 1 : bi xs
:}
bi [[3],[2],[]]

-- returns the distances themselves, not the points
-- hopefully resolved below
:{
let findFurthest l = map (\x -> sqrt $ normSq $ distPtAndLine x left right) fl
      where fl = removeEmptys $ aboveL l
            left = minXCoord p
            right = maxXCoord p
            p = pairsL l
:}

findFurthest l2

l2
al
bl

max1
min1

further [1,2] [0,1] max1 min1

pairsL l2
max1
min1


(pairsL l2)

foldr (\x y -> further x y max1 min1) max1 (pairsL l1)

foldr (\x y -> further x y max1 min1) max1 (belowL l1)

belowL l1

dist1 []

:{
let further p1 p2 a b
      | dist1 > dist2 = p1
      | otherwise = p2
        where dist1 = distance p1 a b
              dist2 = distance p2 a b
:}

-- max1,min1 have been defined for local testing


let distance x y z = sqrt $ normSq $ distPtAndLine x y z

((** 0.5) $ normSq $ distPtAndLine [1,0,1] [1,2,-1] [2,0,3])   == 2 * (14 ** 0.5) /7

distPtAndLine [1,2] [4,3] [-3,2]


-- so far abolveL -> filter /= [] -> map distPtAndLine -> max -> insidetriangle? -> recurse over last three steps

al
bl

:t maxXCoord

maxXCoord $ pairsL l2
minXCoord $ pairsL l2

maxXCoord $ pairsL l3
minXCoord $ pairsL l3

l2

let l2 = take 20 $ randomList 303
let l3 = take 20 $ randomList 3033

let removeEmptys xs = filter (/= []) xs

l2

removeEmptys al
removeEmptys bl






:t maxXCoord
:t minXCoord

:{
let pairsL [] = []
    pairsL (x:y:xs) = [x,y] : pairsL xs
:}




-- p t = p1 + (p2 - p1) t

let vecAdd = zipWith (+)
let vecSub = zipWith (-)
let vecScalarMult c = map (*c)


-- t = 0 implies p1
-- t = 1 implies p2

-- vector equation through p1 & p2 in parametric form, parameter t
let p t p1 p2 = vecAdd p1 (vecScalarMult t (vecSub p2 p1))

asdf t = p t

let ip = (foldr (+) 0 .) . (zipWith (*))

let normSq x = ip x x


-- think p - the parametrized line above
-- note p2MinusP1 is the normal vector to the hyperplane, a, on wikipedia
:{
let distPtAndLine p p1 p2 = vecSub pMinusp1 (vecScalarMult projectedMagnitude p2MinusP1)
      where
        projectedMagnitude = ipPP1andP2P1 / normP2P1
        ipPP1andP2P1 = ip pMinusp1 p2MinusP1
        normP2P1 = normSq p2MinusP1
        pMinusp1 = (vecSub p p1)
        p2MinusP1 = (vecSub p2 p1)
:}

distPtAndLine [1,0,1] [1,2,-1] [2,0,3]

-- verification this is correct, up to stackoverflow
((** 0.5) $ normSq $ distPtAndLine [1,0,1] [1,2,-1] [2,0,3])   == 2 * (14 ** 0.5) /7

(2 * (14 ** 0.5) /7) ** 2
































(map realToFrac [[1,4],[0.5,0.9],[2,3]])

realToFrac 3.0

[[1,4],[0.5,0.9],[2,3]]

[3,4.0]




-- remember, infinity needs to be compared to nonIntegral number
-- hence the realToFrac
foldr max (-infinity) (map (/2) [1..5])
foldr min (infinity) (map (/2) [1..5])

foldr min (infinity) (map realToFrac [1..5])

:t [1..4]

:t (map realToFrac [1..4])

:t (realToFrac 1)

foldr (+) 0 [1..5]


:t (3.0)

3.0 > (-infinity)




