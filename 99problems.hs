-- last element of list

:{
let lE [x] = x
    lE (x:xs) = lE xs
:}

lE[1..20]


const 1 23

const id 23 3

-- 2nd to last element

:{
let slE [x,y] = x
    slE (x:xs) = slE xs
:}

slE [1..20]


kth elem of list

:{
let ka 0 (x:xs) = x
    ka k (x:xs) = ka (k-1) xs
:}

ka 3 [11..22]

let fd x y = ka y x

fd [1..20] 3

-- wasn't working because i had the two noncommutative types

:{
let kE :: [a] -> Int -> a
    kE (x:xs) 0 = x
    kE (x:xs) y = kE xs (y-1)
:}


:{
let elementAt' :: [a] -> Int -> a
    elementAt' (x:_) 1  = x
    elementAt' [] _     = error "Index out of bounds"
    elementAt' (_:xs) k
      | k < 1           = error "Index out of bounds"
      | otherwise       = elementAt' xs (k - 1)
:}

elementAt' 3 [1..23]

:{
let myRev [] y = y
    myRev (x:xs) y = myRev xs (x:y)
:}

let myRev1 x = myRev x []

myRev1 [1..23]

myRev [1..23] []

-- their version

reverse :: [a] -> [a]
reverse list = reverse' list []
  where
    reverse' [] reversed     = reversed
    reverse' (x:xs) reversed = reverse' xs (x:reversed)

let listSubtract = zipWith (-)

listSubtract [1..30] [101..130]

length [1..20]

nZeros x:xs length(x) = 0 : nZeros xs

let palindrome' xs = and $ zipWith (==) xs (reverse xs) -- same, but easier

palindrome' [1,2,3,2,1]

isPalindrome (x:xs) = if (listSubtract x (reverse x) == 0)
                         then




-- (**) Eliminate consecutive duplicates of list elements.


:{
let elimDup [x,y] = if (x == y)
                     then [x]
                     else [x,y]
:}

elimDup [2,3]
elimDup [2,2]

:{
let elimDup1 x y = if (x == y)
                     then [x]
                     else [x,y]
:}

elimDup1 1 1

let ff [x,y,z] = elimDup1 x elimDup1

ff [1,1,1]

:{
let myDrop xs 0 = xs
    myDrop (x:xs) y = myDrop xs (y-1)
:}

myDrop [1..50] 45


myDropWhile
myDropWhile f (x:xs) = f x : myDropWhile xs


:{
let dropIf f x = if (f x == True)
                    then []
                    else [x]
:}

dropIf f xs'@(x:xs)
  | f x = dropIf f xs
  |

Null


dropWhile (== 1) [1,1,1,1,2,3,4]

map (dropWhile (== 1)) [1,1,2,1,1,111,1,1,13,3,3]

Id

:{
let compress (x:ys@(y:_))
        | x == y    = compress ys
        | otherwise = x : compress ys
    compress ys = ys
:}

compress [1,0,1,1,1,0,0,0]


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

compressAndCdr [0,0,1,0,1,1,1,0,0]



(3 == 3) == True


dropIf (<3) 3



dropIf (<3) 4
dropIf (<3) 2

map (dropIf (<3))


:{
let dropWhile               :: (a -> Bool) -> [a] -> [a]
    dropWhile _ []          =  []
    dropWhile p xs@(x:xs')
                | p x       =  dropWhile p xs'
                | otherwise =  xs
:}

dropWhile (<41) [1..40]

dropWhile even [1..10]

even 3

-- (**) Eliminate consecutive duplicates of list elements.


elimConseq



foldr (+) 0 [1..10]

foldr elimDup1 0  [1..30]

:{
mFO f n [] = n
mFO f n (x:xs) = f x ( mFO f n xs)
:}


foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

:{
let quicksort :: (Ord a) => [a] -> [a]
    quicksort [] = []
    quicksort (x:xs) =
        let smallerSorted = quicksort [a | a <- xs, a <= x]
            biggerSorted = quicksort [a | a <- xs, a > x]
        in  smallerSorted ++ [x] ++ biggerSorted
:}

quicksort [1,4,2,6,32,6,1,7,0]

-- from the haskell wiki

let quicksort :: Ord a => [a] -> [a]
    quicksort []     = []
    quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
        where
            lesser  = filter (< p) xs
            greater = filter (>= p) xs

let qs [] = []
    qs (x:xs) = (qs lt) ++ [x] ++ (qs gt)
      where
        lt = filter (< x) xs
        gt = filter (>= x) xs





