{-
This module contains koans for the most basic functions
in Haskell
-}
module BasicFunctions (koans) where

import Test.Hspec (Spec, describe, it)
import Test.HUnit (assertBool, assertEqual)

import Util


:{
let mm f [] = []
    mm f (x:xs) = f x : mm f xs
:}

mm (++ "abc") ["a","b"]


sin (3)
sin (pi)

let mysin op hyp = op / hyp

let sqrt2 = 2 ** (1/2)

let sqrt2Over2 = sqrt2 / 2

sqrt2Over2

(1/2)

sin(pi/4)

mysin sqrt2Over2 sqrt2Over2

data NonEmpty a = a :| [a]

let isNE y = if (y == [])
                return

[] == []

NonEmpty [3,4]

main $ 2 :| [3]

let mymap :: (x -> y) -> [x] -> [y]

:{
let mymap :: (x -> y) -> [x] -> [y]
mymap [] _ = []
mymap f (x:xs) = f x : mymap f xs
:}


:{
let map :: (a -> b) -> [a] -> [b]
    map _ [] = []
    map f (x:xs) = f x : map f xs
:}



data (RealFloat a) => Complex a = !a :+ !a  deriving (Eq, Text)




:{
let conjugate               :: (RealFloat a) => Comlex a -> Complex a
    conjugate (x:+y)        =  x :+ (-y)
:}




data (RealFloat a) => Complex a = !a :+ !a  deriving (Eq, Text)


let errorone = properFraction . snd

let errorone = snd . properFraction


let a = 34.23

let b = 334.33

let errordiff a b = errorone a - errorone b

let c = errordiff 24.5 324.6


let d = errordiff a b

:{
let foldl' :: (a -> a -> a) -> [a] -> a
    foldl' _ [] = []
    foldl' f (x:xs) = f x foldl' f xs
:}


:{
let foldr f z []     = z
    foldr f z (x:xs) = f x (foldr f z xs)
:}




let plus3 x = x + 3

let times2 x = 2 * x

times2 $ plus3 3


let twotimesxplus3 = times2 . plus3

twotimesxplus3 3



:{
let zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
    zipWith' _ [] _ = []
    zipWith' _ _ [] = []
    zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
:}



:{
let vectoradd [] [] = []
    vectoradd (x:xs) (y:ys) = x + y : vectoradd xs ys
:}

:{
let scalarvectormult c [] = []
    scalarvectormult c (x:xs) = c * x : scalarvectormult c xs
:}


:{
let dotproduct c [] [] = c
    dotproduct c (x:xs) (y:ys) = x * y + dotproduct xs ys
:}

dotproduct (x:xs) (y:ys) = zipWith' (*) . foldr 0 (+)

let dotproduct = zipWith (*) . foldr 0 (+)

let asdf = foldl (+) 0

asdf [12,3]

let fdsa = zipWith (*)

fdsa [1,2] [34,32]

-- correct version
let dp x y = asdf (fdsa x y)

-- why doesn't this work ?
-- ahhh! its application, not composition.  see below $
let dp x y = (asdf) . (fdsa)


-- correctomundo
let dpp x y = asdf $ fdsa x y

dpp [32,3] [79,456]

dp [1,0] [1,1]

let dp = asdf . zipWith (*)

let appliedDotProduct x = dpp $ x

let mapappliedDotProduct x y = map (appliedDotProduct $ x) y


mapappliedDotProduct [23,2] [[3,4][432,2]]


let jk = appliedDotProduct [3,4]

let projection x y = map (mapappliedDotProduct x) y

let projection x y = map (appliedDotProduct x) y


projection [3,4] [[36,64],[76,5]]
projection [3,4,5] [[36,63,3],[36,64,3],[76,5,3]]

projection [1,1] [[1,0],[0,1]]

projection [1,0] [[0,1]]

[2.5,5,..,10]

map (*2.5) [1..6]



:{
let
    componentVectorProduct _ [] [] = []
    componentVectorProduct f (x:xs) (y:ys) = f x y : componentVectorProduct f xs ys
:}

componentVectorProduct (*) [2,3] [4,3]
componentVectorProduct (^) [2,3] [4,3]

let myself x y = componentVectorProduct (*) x y
myself [2,3] [4,3]



:{
let
    listreverse [] = []
    listreverse (x:xs) = listreverse xs : [x]
:}

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []



:{
let take 0 xs = []
    take n (x:xs) = x : take (n-1) xs
:}

:{
let leave 0 xs = xs
    leave n (x:xs) = leave (n-1) xs
:}

take 3 [3,4,456,432,43,44,3]

leave 3 [3,4,456,432,43,44,3]


let splitat n xs = (take n xs, leave n xs)


splitat 3 [3,4,456,432,43,44,3]


import Data.List

import Data.Maybe
import Data.Bool

:t elemIndex

elemIndex 3 [3,43,3,423,34,456]


if (3 - 3 == 0) 3 4

let myVar = 45

if True (myVar + 56565656) (myVar + 3)


:{
let a = if True
           then 3
           else 4
:}

a

:{
let aa x y  = if ((x-y) == 0)
              then 3
              else 4
:}


-- b determines the maximum

:{
let b x y  = if ((x-y) > 0)
              then x
              else y
:}

-- c determines the min
-- question: how to combine these two definitions into one?

:{
let c x y  = if ((x-y) < 0)
              then x
              else y
:}

:{
let d x y  = if (x > y)
              then x
              else []
:}

d 3 4

:{
let isEmpty x = if (x == [])
              then True
              else False
:}


-- longest Consecutive Sequence

:{
let conseqInts x y = if (y - x == 1)
                then True
                else False
:}

-- alternate syntax
-- Test if the two integers are increasingly consecutive

:{
let csqI x y
        | (y - x == 1) = True
        | otherwise   = False
:}

csqI 3 4

conseqInts 3 4

-- Approach to problem:  how to find the longest sequence of 1s in a list of 1s and zeros? [1,0,1,1,0,1,1,1]

-- Recursively establish which pairs in a list are consecutive.
-- Note that if we have 5 consecutive integers, this will amount to 4 consecutive Trues
-- isConsec :: Int a => [a] -> [Bool]
-- length(_outputList_) = length(_inputList_) - 1

:{
let isConsec [x] = []
    isConsec (x:xs) = conseqInts x (head xs) : isConsec xs
:}

isConsec [1..5]
isConsec ([1..5] ++ [3,4,3,2,1])

:{
let booleansToBinary x
                     | x == True = 1
                     | otherwise = 0
:}

booleansToBinary True

let a = isConsec [1..5]
let b = isConsec ([1..5] ++ [3,4,3,2,1])

let listBoolsToListBinary = map booleansToBinary

listBoolsToListBinary a
listBoolsToListBinary b

-- get rid of sequences of zeros, indendent of length, and replace with a single zero

:{
let compressZeros (x:ys@(y:_))
          | (x == y) && (x == 0) = compressZeros ys
          | otherwise = x : compressZeros ys
    compressZeros ys = ys
:}

compressZeros [0,0,1,0,1,1,1,0,0,0]

-- cdr if first element of a list is zero

:{
let cdrIfFirstZero y@(x:xs)
                  | x == 0 = xs
                  | otherwise = y
:}

cdrIfFirstZero [0,0,1,0,1,1,1,0,0,0]
cdrIfFirstZero [1..3]

let compressZerosAndCdr = cdrIfFirstZero . compressZeros

compressZerosAndCdr [0,0,1,0,1,1,1,0,0]

:{
let countZeros [] n = n
    countZeros (x:xs) n
              | x == 0 = countZeros xs (n+1)
              | otherwise = countZeros xs n
:}

countZeros [0,0,1,0,1,1,1,0,0] 0


-- map the consective sequences to their own lists

:{
let iCMod [] ys = ys
    iCMod (x:xs) (y:ys) = if (x == 1)
                             then iCMod xs (([x] ++ y):ys)
                             else y :iCMod xs ys
:}

iCMod [1,0,1,1] [[],[]]

iCMod [1,0,0,1,1,0,1,1,1,0,1] [[],[],[],[],[]]

iCMod [1,1,0,0,1,1,1,0,1,0,1,0,1] [[],[],[],[],[]]

:{
let iCMod [] ys = ys
    iCMod (x:xs) (y:ys) = if (x == 0)
                             then iCMod xs ys
                             else ([x] ++ y) : iCMod xs (y:ys)
:}

iCMod [1,1] [[]]

iCMod [0,0,1,1,0,1,1,1,0,1] [[],[],[],[],[]]

iCMod [1,1,0,1,1,1,0,1,0,1,0,1] [[],[],[],[],[]]

-- gEL === generate list of empty lists

:{
let gEL m 0 = []
    gEL m n = m : gEL m (n-1)
:}

gEL [] 3

-- can be defined commutatively
:{
let f1 0 x = []
    f1 n x = x : f1 (n-1) x
:}

-- map length of a list of lists

let mapLength = map length

mapLength [[1..3],[4..53]]

:{
let max x y
      | x > y = x
      | otherwise = y
:}

max 3 98

-- maximum of a list

let maxL = foldr max 0

maxL [3,4,56,2]

:{
let generateListofNEmptyLists 1 = []
    generateListofNEmptyLists n = [] : generateListofNEmptyLists (n-1)
:}

:{
let gEL m 1 = []
    gEL m n = m : gEL m (n-1)
:}

gEL 3 4

3 : 3 : []

:{
let gEL :: Int a => a -> [b]
    gEL 1 = []
    gEL n = n : gEL (n-1)
:}

:{
let gEL 1 = []
    gEL n = "a" : gEL (n-1)
:}

gEL 3


let

:{
let gEL 1 = []
    gEL n = [n] ++ gEL (n-1)
:}

:{
let sqNList 0  = []
    sqNList n  = n*n : sqNList (n-1)
:}

:{
let sqNList :: Int -> [Int]
    sqNList 0  = []
    sqNList n  = n : sqNList (n-1)
:}

:{
let sqNList :: (Num a, Ord a) => a -> [a]
    sqNList n = helper 1
        where helper i | i > n = []
                       | otherwise = i*i : helper (i+1)
:}


:{
let sqNList :: (Num a, Ord a) => a -> [a]
    sqNList n = helper 1
        where helper i | i > n = []
                       | otherwise = i*i : helper (i+1)
:}


sqNList 4

generateListofNEmptyLists 3


let f x y = x : y

f 3 []
f [] []

:{
let f 0 x y = []
    f n x y = x : f (n-1) x y
:}

f 3 [] []


-- kinda working, but has a weird type signature

:{
let trueList [] = []
    trueList (x:xs) = if (x == True)
                         then trueList xs
                         else [False]
:}

trueList (isConsec [1..5])

trueList ([True,False] ++ isConsec [1..5])

-- correct version!

:{
let trueList [] = True
    trueList (x:xs) = if (x == True)
                         then trueList xs
                         else False
:}

let toList x = [x]
toList 3

map toList [1..5]

--new, [partially] correct version! instead of true false we return the length of the first n Trues
:{
let trueList [] n = n
    trueList (x:xs) n m = if (x == True)
                           then trueList xs n+1
                           else n
:}

False : [True]

let a = [True]

False :a

-- different approach.. try to seperate into a list of truelists

let lOfTls (x:xs) = if (x == True)
                       then [x] :

[3] : [[4]]

3 : [4]


-- trying to figure out how to do this, no success
:{
let trueLists [] n m = m
    trueLists (x:xs) n m = if (x == False)
                            then trueLists xs n m
                            else trueList (x:xs) n
:}

trueLists afd 0


let afd = isConsec ( [-1] ++ [1..5] ++ [3] ++ [3..9])

let af = isConsec ( [-1] ++ [1..5])

af

trueList af 0


-- mutual recursion

:{
let even :: Int -> Bool
    even 0 = True
    even n = odd (n - 1)
    odd :: Int -> Bool
    odd 0 = False
    odd n = even (n - 1)
:}

even 3




:{
let isNonEmpty x = if (x /= [])
              then True
              else False
:}

3 /= 3

isNonEmpty [3..4]

map isNonEmpty i



isnm []
isnm [3]





filter (>3) [1..40]

let filterGreaterThanN n = filter (>n)

filterGreaterThanN 34 [1..50]

filter2ndComponent


let mapFilterGTN (x:xs) = map (filterGreaterThanN xs)

i


mapFilterGTN i

-- divide the a list of points based off which side of the line they are on

map (++) [

:{
let combinationsA x [] = []
    combinationsA x (y:ys) = [x,y] : combinationsA x ys
:}

combinationsA 3 [1..3]

let combB x y = combinationsA y x

combB [1..3]

head $ map (combB [1..3]) [3..5]

let i = head $ map (combB [1..3]) [3..5]

i


let twoPtSlope (x0,y0) (x1,y1) = (y1 - y0) / (x1 - x0)

twoPtSlope (1,2) (4,3)

let m = zipWith (-)

m [1..5] [5..9]

let p1 = [2,0,3]
let p2 = [1,2,-1]

let

let iPDV p p1 p2 = innerProduct (m p p1) (m p2 p1)

let sqrt' x = x ** (1/2)

-- correct but its the norm squared so doesn't apply in this case
let normV p = sqrt' $ innerProduct p p
normV [2,2]

-- finally, this is correct

let niPDV p p1 p2 = (iPDV p p1 p2) / (innerProduct (m p2 p1) (m p2 p1))

niPDV p0 p2 p1

let dfl p0 p1 p2 = (m (m p0 p1) (scalarMultiplication (niPDV p0 p1 p2) (m p2 p1)))

(scalarMultiplication (niPDV p0 p1 p2) (m p2 p1))


let dflCorrect p0 p1 p2 = normV (dfl p0 p1 p2)

dflCorrect p0 p1 p2

let p0 = [1,0,1]
let p1 = [2,0,3]
let p2 = [1,2,-1]

let q0 = [-5,1,0,1]
let q1 = [2,2,0,3]
let q2 = [3,1,2,-1]

dflCorrect q0 q1 q2




-- https://math.stackexchange.com/questions/1300484/distance-between-line-and-a-point

let test = dfl p0 p2 p1

normV test

normV [-4,-6,-2]

(2 * sqrt 14) / 7


innerProduct [1..3] [1,0,2]

let p0 = [1,0,1]
let p0 = [1,0,1]

(m p0 p2)

innerProduct (m p0 p2) exslope

sqrt' $ innerProduct exslope exslope

let exslope = m p1 p2

exslope


let scalarMultiplication t = map (*t)

let vectorAddition = zipWith (+)

let line t m p = vectorAddition p (scalarMultiplication t m)

let ltMp0 ln point = m ln point

ltMpo

let innerProduct x y = sumList $ fdsa x y

innerproduct p2 p1 p

normalizedProjection p p1 p2 = (m p p1) - (innerproduct (m p p1) (m p2 p1) / (innerproduct


let sumList = foldl (+) 0

sumList [12,3]

let fdsa = zipWith (*)

fdsa [1,2] [34,32]

-- correct version
let dp x y = asdf (fdsa x y)



:{
let myZip f [] [] = []
    myZip f (x:xs) (y:ys) = f x y : myZip f xs ys
:}

myZip (+) [1..3] [4..6]


distancePtToLine a b




c (-3) 5

c 5 (-3

b 5 4
b 5 5
b 4 5
map (b 5) [1..10]

c 5 4
map (c 5) [1..10]

-- its just a fold, i think
--
foldr (+) 0 [1..23]

foldr b 0 [2,3,4]

foldr b 0 [5,2,3,4]

[-3..4]

(-1) * 3

let negList = map (*

[-3..(-1)]


-- the proper edge case value must either be some construct defined as a -infinity or <= gLB

foldr b (-39) [-3..(-1)]



aa 3 4






aa 4 4




filter (<3) [3,34,2,1]

let fst (x:xs) = x

let car (x:xs) = x

fst [4,3,4]

let cdr (x:xs) = xs

cdr [3,4,5]

let snd (x:xs) = fst xs

snd [3,4,43,34]


-- for even input
:{
let evens [] = []
    evens (x:xs) = x : evens (cdr xs)
:}

evens [43,4,355,7]

:{
let odds [] = []
    odds (x:xs) = (car xs) : odds (cdr xs)
:}

odds [43,4,355,7]

odds [43,4,355,7]


:t oAE

oAE :: [a] -> [a]
let oAE xs = (evens xs) ++ (odds xs)

oAE [6,75,43,5,54,3]

:t oddsAndEvens


oddsAndEvens :: [t] -> ([t], [t])
let oddsAndEvens xs = (evens xs, odds xs)

let binaryTupleToList (x,y) = x ++ y

binaryTupleToList (3,4)

binaryTupleToList ([4,5],[5,6])

:{
let consTwoLists [] ys = ys
    consTwoLists (x:xs) ys = x : consTwoLists xs ys
:}

consTwoLists [4,5] [5,6]


let complexAdd (x1,x2) (y1,y2) = (x1+y1,x2+y2)

complexAdd (3,4) (4,3)

let complexMult (x1,x2) (y1,y2) = (x1*y1-x2*y2,x1*y2 + x2 * y1)

complexMult (3,4) (4,3)

complexMult (3,4) (4,6)

let exponentialComplexMult (r,theta1) (s,theta2) = (r*s,theta1 + theta2)

exponentialComplexMult (3,pi) (4,pi/2)

let cartesianToExponentialComplex (x,y) = (sqrt(x^2 + y^2) , atan(y/x))

cartesianToExponentialComplex (3,3)
polar (3:+3)

let tupleToComplex (x,y) = x:+y

let xe = tupleToComplex (cartesianToExponentialComplex (3,3))

let as = (cartesianToExponentialComplex (3,3))

let ex = polar (3:+3)

tupleToComplex as - tupleToComplex ex


(polar (3:+3))

cartesianToExponentialComplex (3,3) - polar (3:+3)

cartesianToExponentialComplex (3,0)

cartesianToExponentialComplex (0,3)

-- I'm here playing with the complex Library provided
import Data.Complex

let myCom = 3 :+ 4
let myCom2 = 3.3 :+ 5
myCom + myCom2
myCom - myCom2

let exponentialToCartesianComplex (x,y) = (x * cos(y),x * sin(y))

exponentialToCartesianComplex (3,pi/4)
exponentialToCartesianComplex (2,pi/8)


sqrt(3)
cos(pi)

3 * pi /2


3 : [4] ++ [5]

:t binaryTupleToList


binaryTupleToList (oddsAndEvens [3,4,5,65,75,3])

let c = oddsAndEvens [3,4,5,65,75,3]

binaryTupleToList c

oddsAndEvens [43,4,355,7]

odds [





:{
let a n x y = if (<n)
                 then x
                 else y
:}

a (<0) 3 -4


:{
let evens 0 _ = []
    evens n (x:xs) = x : evens (n-2) xs
:}



evens 4 [3,34,3,3,43,4345]
evens 6 [3,34,3,3,43,4345]

oddsAndEvens n (x:xs) = (x: (oddsAndEvens n-2 xs), oddsAndEvens


filterByIndex f (x:xs) =




splitWithFilter






take n _      | n <= 0 =  []
take _ []              =  []
take n (x:xs)          =  x : take (n-1) xs

drop n xs     | n <= 0 =  xs
drop _ []              =  []
drop n (_:xs)          =  drop (n-1) xs

let splitAt n xs           =  (take n xs, drop n xs)

-- remember, the smallest index of the polynomial (e.g. the constant term) is the end of the list

:{
let hornersRule [] n = 0
    hornersRule (x:xs) n = x + n * hornersRule xs n
:}

hornersRule [1..3] 3


polynomialAddition x y = zipwith (+) (zerpopadded x y) (nonzeropadded x y)


:{
let pA [] [] = []
    pA [] (y:ys) = y : ys
    pA (x:xs) [] = x : xs
    pA (x:xs) (y:ys) = x + y : pA xs ys
:}

works on inputs independent of order now

pA [1..5] [1..5]

pA [1..3] [1..5]

pA [1..7] [1..5]

-- multiply polynomial by monomial

let multPByConstant x = map (* x)

multPByConstant 3 [1..4]

let multPByX xs = 0 : xs

let multPByNX n xs = 0 : multPByConstant n xs

let mpbnx n xs = multPByConstant n .multPByX $ xs

let mpbnx1 n xs = multPByConstant n .multPByX $ xs

let mpbnx2 n = multPByX . multPByConstant n


mpbnx2 3 [1..3]

mpbnx1 3 [1..3]

multPByX [1..34]

multPByNX 3 [1..34]


let nx2 = multPByX . multPByX


let nc n f = foldr (.) id (replicate n f)

let xxx = nc 3 (+3)

xxx 4

let ncc n f = foldr (.) id $ replicate n f

-- definted with my own replicate in the most compact way I can think of

let ancc n f = foldr (.) id $ replicate' n f

let mPbyXtoTheN n = ancc n multPByX

mPbyXtoTheN 3 [1..23]

ancc 4 (+34) 1


:{
let polyMult n [] ys = []
    polyMult n (x:xs) ys = multPByConstant x ys : polyMult (n+1) xs ys
:}

polyMult 0 [1..3] [2,3,4]

-- finally this works!

:{
let zeroPadPoly _ [] = []
    zeroPadPoly n (x:xs) = mPbyXtoTheN n x : zeroPadPoly (n+1) xs
:}

[3] : [4] : [[]]

let abc = polyMult 0 [1..3] [2,3,4]

let aabc = polyMult 0 [-1..3] [2,3,4]

zeroPadPoly 0 aabc

pA [1..4] [2..5]

let p = [-1,3,4,5,6] : abc


-- Finally, a way to add all the polynomials!
-- foldr pA [0] (polyMult poly1 poly2)

foldr pA [0,0] p




(!!) [1..23] 2

[30..34]

[-1..2]



-- id works as the identity for function composition

(+3) . id $ 4

ncc 3 (+3) 4

replicate 34 3


:{
let replicate' 0 f = []
    replicate' n f = f : replicate' (n-1) f
:}

replicate' 34 3

replicate 34 (+3)

:{
let ncomp f 1 = id
    ncomp f n = f . ncomp f n-1
:}

nx2 [1..23]

-- how about breaking this down into steps where we fold with a polynomial addition all the polynomial products

polynomialMultiplication [] ys = ys
polynomialMultiplication (x:xs) ys = multPByConstant x ys : polynomialMultiplication (multPByX xs) ys




splitAt 3 [3,43,34,34,5,6,3]




take 3 [3,43,34,34,5]


drop 3 [3,43,34,34,5]





let kk y = map jk y

kk [[36,64],[76,5]]

map jk [[36,64],[76,5]]

jk [3,4]



let dotproduct (x:xs) (y:ys) = foldl (+) 0 . (zipWith (*) (x:xs) (y:ys))






c-d


a = array (1,100) ((1,1) : [(i, i * a!(i-1)) | i <- [2..100]])


(3.2 - 3.1) - (errorone 3.2 - errorone 3.1)


3.4 - errorone 3.4

properFraction


properFraction


properFraction




:{
let round::
    round
:}

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

:{
let filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
:}


:{
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0
:}

:{
f x = 3 * x


:}

koans :: Koan
koans = describe "BasicFunctions" $ do
    -- * Arithmetic Operators
    koanPlus
    koanMinus
    koanProd

    -- * Logical Operators
    koanAnd
    koanOr
    koanNot

    -- * Basic Parsing functions
    koanRead
    --, koanReads

    -- * Functional utilities
    koanId

----------------------------------------------------------------------
--
-- Arithmetic Operations
--
----------------------------------------------------------------------

koanPlus :: Koan
koanPlus = koan "(+) function" $ do
    -- REPLACE: replaceValue with correct value
    -- let result = replaceValue "(+)"

    -- SOLUTION: Check assignment to result
    let result = 2 + 2
    assertEqual "use (+) function" 4 result

koanMinus :: Koan
koanMinus = koan "(-) function" $ do
    -- REPLACE: replaceValue with correct value
    let result = replaceValue "(-)"
    assertEqual "use (-) function" 10 result

koanProd :: Koan
koanProd = koan "(*) function" $ do
    -- REPLACE: replaceValue with correct value
    let result = replaceValue "(*)"
    assertEqual "use (*) function" 6 result

----------------------------------------------------------------------
--
-- Logical Operators
--
----------------------------------------------------------------------

koanAnd :: Koan
koanAnd = koan "(&&) function" $ do
   -- REPLACE: replaceValue with correct value
   let result = replaceValue "(&&)"
   assertEqual "use (&&) function" False result

koanOr :: Koan
koanOr = koan "(||) function" $ do
   -- REPLACE: replaceValue with correct value
   let result = replaceValue "(||)"
   assertEqual "use (||) function" True result

koanNot :: Koan
koanNot = koan "not function" $ do
   -- REPLACE: replaceValue with correct value
   let result = replaceValue "not"
   assertEqual "use (not) function" False result

----------------------------------------------------------------------
--
-- Basic Transformation from String to types
--
----------------------------------------------------------------------

koanRead :: Koan
koanRead = koan "read function" $ do
    -- REPLACE: replaceValue with correct value
    let result = replaceValue "read"
    assertEqual "use read function" 1566 result

koanReads :: Koan
koanReads = koan "reads function" $ do
    -- REPLACE: replaceValue with correct value
    let result = replaceValue "reads"
    assertEqual "use reads function" [(1566, " other string")] result

----------------------------------------------------------------------
--
-- Identity function
--
----------------------------------------------------------------------

koanId :: Koan
koanId = koan "id function" $ do
    -- REPLACE: replaceValue with correct value
    let result = replaceValue "id"
    assertEqual "use id function" 1566 result




