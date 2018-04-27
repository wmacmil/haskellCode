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





