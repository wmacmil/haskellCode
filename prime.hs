
:{
let rndRoot :: Integral a => a -> a
    rndRoot = round . sqrt . fromIntegral
:}

:{
let oneToRndRoot :: Integral a => a -> [a]
    oneToRndRoot x = [1..rndRoot(x)]
:}

:{
let oTRR x = [1..rr(x)]
      where rr = round . sqrt . fromIntegral
:}

oTRR 38


:{
let modulo :: (Ord p, Num p) => p -> p -> p
    modulo x y
      | n < 0 = x
      | otherwise = modulo n y
      where n = x - y
:}

:{
let mapMod :: (Num b, Ord b) => b -> [b] -> [b]
    mapMod x = map (modulo x)
:}

:{
let mapModToRndRoot :: Integral a => a -> [a]
    mapModToRndRoot n = mapMod n (oneToRndRoot n)
:}

:{
let mapModToRndRoot :: Integral a => a -> [a]
    mapModToRndRoot n = mapMod n (oTRR n)
:}

mapModToRndRoot 34
mapModToRndRoot 38

mapModToRndRoot 2

mapModToRndRoot 3

-- got it, I think!

:{
let zeroInList (x:y:xs) = go (y:xs)
      where
        go [] = False
        go (y:xs)
          | y == 0 = True
          | otherwise  = go xs
:}



zeroInList [0,1]

zeroInList [0,1,2,3,2]
zeroInList [0,10,0,2,3,2]

mapModToRndRoot 43

zeroInList $ mapModToRndRoot 47

zeroInList [0,1,1,3,3,1,1]

zeroInList $ mapModToRndRoot 43

zeroInList $ mapModToRndRoot 3

zeroInList $ mapModToRndRoot 2

let zILMMTRR x = zeroInList $ mapModToRndRoot x

let zILMMTRR = zeroInList . mapModToRndRoot

:t zILMMTRR

zILMMTRR 42

zILMMTRR 43

zILMMTRR 44

map zILMMTRR [42,43]

map zILMMTRR [3..10]

let mZ = map $ zILMMTRR

let toList x = [x]

let we = map toList [1..10]

zipWith (:) [1..10] we

zip [7, 8, 2] "cat"


mzip [7, 8, 2] "cat"

(\x y -> (x,y)) 3 'b'

(\x y -> (x,y))

let zip' = zipWith (\x y -> (x,y))

:{
let mzip [] _ = []
    mzip (x:xs) (y:ys) = (x,y) : mzip xs ys
:}


1 : [1]

let primes x = zipWith (:) x $ (mZ x)

[2..10]

map (zeroInList $ mapModToRndRoot) [3..10]

