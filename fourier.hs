-- seperate a list into even and odd indices
--

:{
let evens [] = []
    evens (x:y:xs) = x : evens xs
:}

evens [1..10]
evens ['a'..'z']

:{
let odds [] = []
    odds (x:y:xs) = y : odds xs
:}

odds [1..10]
odds ['a'..'z']

-- compute F_n/2 (f_even)
-- compute F_n/2 (f_odd)

-- lets try to generate the DFT matrix
-- start out with simpler matrices

-- basis vectors of length n with 1 at index j


:{
let basisVector 0 _ = []
    basisVector n 1 = 1 : basisVector (n-1) 0
    basisVector n j = 0 : basisVector (n-1) (j-1)
:}

basisVector 3 1

map (basisVector 3) [1..3]

-- identity matrix n by n

let id n = map (basisVector n) [1..n]
id 5

-- vector multiplication, inner product, dot product, etc
-- sum (x_i * y_i)

let innerProduct = (foldr (+) 0 .) . (zipWith (*))

-- matrix vector multiplication
-- x_j - innerProduct M_i x

let matVecProduct m v = map (innerProduct v) m

matVecProduct [[2,3],[4,3]] [1,2]

-- note that we should transpose our second input to get it into the proper form for this definition
-- e.g. m2 must be the pretransposed

let matMatProduct m1 m2 = map (matVecProduct m1) m2

matMatProduct [[2,3],[4,3]] [[2,3],[4,3]]

matMatProduct [[1..5],[2..6]] [[1..5],[2..6]]

innerProduct [2,3] [2,3]
innerProduct [2,3] [4,3]

transpose [[10,11],[20],[],[30,31,32]]

-- the first line is for completeness
-- this only defines transpose of a matrix, not the same as in the list docs

:{
let transpose [] = []
    transpose [[],_] = []
    transpose y@(x:xs) = map head y : transpose (map tail y)
:}

transpose [[2,3],[4,3]]

transpose [['a'..'z'],['a'..'z']]

["a..z"]

transpose [[1..6],[5..10]]
transpose [[1,2,3],[4,5,6]]

[[1,2,3],[4,5,6]] == [[1,2,3],[4,5,6]]


:{
let isSymmetric m
      | m == transpose m = True
      | otherwise = False
:}

isSymmetric [[2,3],[4,3]]
isSymmetric [[2,3],[3,2]]

-- get the (i,j)th elemet of a matrix

-- get the ith element of a list
-- this applies to the ith row of a matrix
-- should refactor to have i first

:{
let ithElem (x:xs) 1 = x
    ithElem (x:xs) i = ithElem xs (i-1)
:}

:{
let ithElem (x:xs) 1 = x
    ithElem i (x:xs) = ithElem (i-1) xs
:}

ithElem [1..10] 3


let ijthElem m i j = ithElem (ithElem m i) j

ijthElem [[1..6],[5..10]] 2 2


let asdkjf x y z w = zipWith (+) [x,y] [z^2,w^3]

asdkjf 2 3 4 4

-- how to have a matrix filled by a formula, e.g. x(i,j) = i^2 + j^2
--how to do this for just a list?

:{
let xi f i 0 = []
    xi f i n = f i : xi f (i+1) (n-1)
:}

-- kind of like having two four loops
-- can we map the j part after? if there's no dependency internal

-- break it up

:{
let xic f i c 0 = []
    xic f i c n = f i c : xic f (i+1) c (n-1)
:}

xic (\x y -> (x^y) + y) 0 4 10
xic (\x y -> (x^y) + y) 0 4 3

-- think this works, but it's messssy

:{
let xij f i j n 0 = []
    xij f i j n m = xic f 1 j n : xij f i (j+1) n (m-1)
:}

xij (\x y -> (x^y) + y) 0 1 3 3

(\x y -> (x^y) + y)

f i j : xij f (i+1) (j+1) n (m-1)


let xi' f = xi f 0

xi' (*38) 6

-- this works with a go embedded, but methinks it is much less readable

:{
let xi f 0 = []
    xi f n = go n 0
      where go n i
              | n == 0 = []
              | otherwise = f i : go (n-1) (i+1)
:}

xi (^3) 3



xi (^3) 0 30




-- lets make a 2d rotation matrix
-- [cos,-sin] [sin,cos]
-- rotD :: degree -> rotation matrix
-- rotVec :: degree -> vector -> vector

let rotD d = [[cosD d,-(sinD d)],[sinD d,cosD d]]

rotD 180

let rotVec d v = matVecProduct (rotD d) v

rotVec 30 [rt3over2,oneHalf]

-- cool, seems to work

let rt3over2 = (3 ** (1/2)) / 2
let oneHalf = 0.5

sinD 30
cosD 30

oneHalf ^ 2 + rt3over2 ^ 2

3 ** 0.4



sin pi

let degToRad d =  d * (2 * pi) / 360

degToRad 180

let sinD = sin . degToRad
let cosD = cos . degToRad

sinD 45


isDiag





