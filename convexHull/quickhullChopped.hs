-- Input : Set of N pts
-- can do it if we define infinity, remember

let infinity = read "Infinity"

let minXCoord = foldr (ltXC (<)) [infinity]
let maxXCoord = foldr (ltXC (>)) [-infinity]

maxXCoord [[1,4],[0.5,0.9],[2,3]]
minXCoord [[1,4],[0.5,0.9],[2,3]]

-- how to represent a line in haskell?
-- y = m x + b
-- b where x = 0
-- m = y2 - y1 / x2 - x1

:{
let l x [x0,y0] [x1,y1] = (x * m) + b
      where m = (y1 - y0) / (x1 - x0)
            b = y0 - (m*x0)
:}

l 3 [-1,-1] [1,1]

-- some floating point error is causing my aboveBelow fcn to mess up. not so sure any more
-- aboveBelow take a pt and a line (represented here by two points)

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

above [0.4,0.9] [0.5,0.9] [2,3]

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

let removeEmptys xs = filter (/= []) xs

:{
let pairsL [] = []
    pairsL (x:y:xs) = [x,y] : pairsL xs
:}

-- in the actual algorithm, we should have pairsL take care of this outside the context of aboveBelow's definition

:{
let aboveBelowL lesOrGr l = removeEmptys $ map (\x -> lesOrGr x min max) p
      where min = minXCoord p
            max = maxXCoord p
            p = pairsL l
:}

let aboveL = aboveBelowL above
let belowL = aboveBelowL below

let al2 = aboveL l2
let bl2 = belowL l2
let max2 = maxXCoord (pairsL l2)
let min2 = minXCoord (pairsL l2)

al2
max2

-- returns the distances themselves, not the points
-- hopefully resolved below

:{
let findFurthest l = map (\x -> sqrt $ normSq $ distPtAndLine x left right) fl
      where fl = aboveL l
            left = minXCoord p
            right = maxXCoord p
            p = pairsL l
:}

findFurthest l2
l2

:{
let further p1 p2 a b
      | dist1 > dist2 = p1
      | otherwise = p2
        where dist1 = distance p1 a b
              dist2 = distance p2 a b
:}

further [1,2] [0,1] max1 min1


foldr (\x y -> further x y min2 max2) min2 al2

-- step 3 in wikipedia specification
let furthest p1 p2 l = foldr (\x y -> further x y p1 p2) min2 l

furthest min2 max2 al2
furthest min2 max2 bl2


filter (insideTriangle) l

:t belowL

insideTriangle l =

import Data.List
intersect [1,2] [1,3,4]

:{
let asdf l =  map (\x -> ((belowList x p1 minl) (belowList x p1 maxl))) l
      where p1 = furthest minl maxl l
            minl = minXCoord l
            maxl = maxXCoord l
:}

-- belowList should really have the line two points definitions internal to it
-- f == below
--

l2

bl below (pairsL l2)

bl below (pairsL l2)
bl2 below (pairsL l2)

bl above (pairsL l2)
bl2 above (pairsL l2)

let eir = bl below (pairsL l2)
let ireo = bl2 below (pairsL l2)

length $ intersect eir ireo

length $ bl below (pairsL l2)
length $ bl2 below (pairsL l2)

:{
let bl f l = removeEmptys $ map (\x -> f x mn fr) l
      where mn = minXCoord l
            mx = maxXCoord l
            fr = furthest mn mx l
:}

:{
let bl2 f l = removeEmptys $ map (\x -> f x mx fr) l
      where mn = minXCoord l
            mx = maxXCoord l
            fr = furthest mn mx l
:}

:t bl

:t below


:t furthest

let furthest p1 p2 l = foldr (\x y -> further x y p1 p2) min2 l

:t minXCoord



:{
let aboveBelowL2 abovOrBelo p = removeEmptys $ map (\x -> abovOrBelo x min max) p
      where min = minXCoord p
            max = maxXCoord p
:}

:t aboveBelowL
:t aboveBelowL2

length $ aboveBelowL2 above (pairsL l2)
length $ aboveBelowL2 below (pairsL l2)

let aboveList = aboveBelowL2 above
let belowList = aboveBelowL2 below

:t aboveList


-- need to define a line segment for this part to work.  accomplish this with intersection


-- max1,min1 have been defined for local testing


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

-- p t = p1 + (p2 - p1) t

let vecAdd = zipWith (+)
let vecSub = zipWith (-)
let vecScalarMult c = map (*c)


-- t = 0 implies p1
-- t = 1 implies p2
-- vector equation through p1 & p2 in parametric form, parameter t
let p t p1 p2 = vecAdd p1 (vecScalarMult t (vecSub p2 p1))

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

let distance x y z = sqrt $ normSq $ distPtAndLine x y z

-- verification this is correct, up to stackoverflow
((** 0.5) $ normSq $ distPtAndLine [1,0,1] [1,2,-1] [2,0,3])   == 2 * (14 ** 0.5) /7

-- could potentiall have type errors on numbers, may want to use realToFrac
