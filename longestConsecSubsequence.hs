-- longest Consecutive Sequence
-- dowload ghci, the :{......:} allows for multiple lines in the REPL
-- . === compose
-- $ === apply
-- : === cons (as in x:xs just represents a list where car (x:xs) = x and cdr(x:xs) = xs
-- @y(x:xs) means that we can refernce our lists x:xs as y in the function body

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

let isConsecToBinary = listBoolsToListBinary . isConsec

isConsecToBinary c

let c = [3,2,1] ++ [1..10] ++ [2,3,5]


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
cdrIfFirstZero . cdrIfFirstZero $ [0,0,1,0,1,1,1,0,0,0]
cdrIfFirstZero [1..3]

let compressZerosAndCdr = cdrIfFirstZero . compressZeros

compressZerosAndCdr [0,0,1,0,1,1,1,0,0]

let

:{
let countZeros [] n = n
    countZeros (x:xs) n
              | x == 0 = countZeros xs (n+1)
              | otherwise = countZeros xs n
:}

countZeros [0,0,1,0,1,1,1,0,0] 0

let ctZeros x = countZeros x 0

ctZeros [0,0,1,0,1,1,1,0,0]

let countCompressedCdredZeros = ctZeros . compressZerosAndCdr



-- gEL === generate list of empty lists

:{
let gEL m 0 = []
    gEL m n = m : gEL m (n-1)
:}

gEL [] 3

let gLEL x = gEL [] x

let gLELofCompressedList = gLEL . countCompressedCdredZeros

gLELofCompressedList [1..45]

-- can be defined commutatively
--
-- :{
-- let f1 0 x = []
--     f1 n x = x : f1 (n-1) x
-- :}


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

let maxMapLength = maxL . mapLength

maxMapLength [[1..3],[4..53]]
