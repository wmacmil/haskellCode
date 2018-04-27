:{
let divide [] = []
    divide (x:xs) = [x] : divide xs
:}

let altDivide = map (:[])
altDivide [1,5,2,65,3,6]

m $ altDivide [1,5,2,65,3,6]

mergetilDone $ divide [1,5,2,65,3,6]

mergetilDone $ divide [1,5,2,65,3,7,6]

let asdf = mergetilDone $ mergetilDone $ divide [1,5,2,65,3,7,6]

asdf

merge (head asdf) (head $ tail asdf)

mergetilDone asdf

-- mergesort
:{
let m [x] = x
    m y@(x:xs) = m (mergetilDone y)
:}



:{
let mergetilDone [] = []
    mergetilDone [x] = [x]
    mergetilDone (x:y:xs) = merge x y : mergetilDone xs
:}

m

:{
let merge x [] = x
    merge [] y = y
    merge (x:xs) (y:ys)
      | x < y = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys
:}

merge [1,3,4,8] [2,5,7]

merge [3] []

merge [] []

-- this is a degenerate selection sort which fails when the initial conditions are such that the last element is the greatest

:{
let selectionSort xs@(x:xs')
      | (mxL xs) == (last xs) = xs
      | otherwise = m :selectionSort rst
        where m = minL xs
              rst = noSmall xs
:}


selectionSort [1,3,2,5,4]

selectionSort [1,3,2,5,4,6]

selectionSort [1..4]

:{
let last [x] = x
    last (x:xs) = last xs
:}

-- get rid of smallest element in list

let noSmall y@(x:xs) = filter (/= minL y) y

3 /= 1

noSmall [1..5]
noSmall [3,1,5]


filter (<3) [1..5]

:{
let qs [] = []
    qs (x:xs) = left ++ [x] ++ right
      where left = qs $ filter (<x) xs
            right = qs $ filter (>x) xs
:}

qs [1,4,3,2]

maxL [1,4,3,2]

:{
let maxL [x,y] = max x y
    maxL (x:y:xs)
      | x > y = maxL (x:xs)
      | otherwise = maxL (y:xs)
:}

let infinity = (read "Infinity")::Double

infinity > 10e1000

3e10


:{
let mxL [x] = x
    mxL (x:xs) = max x $ mxL xs
:}

:{
let minL [x] = x
    minL (x:xs) = min x $ minL xs
:}

:t mxL

mxL [333,4,31,1,33]

:{
let maxmin x y f
      | f x y = x
      | otherwise = y
:}

let max' x y = maxmin x y (>)
let min' x y = maxmin x y (<)



max' 3 4

foldl (max') (-infinity) [1,4,23,2,493,24,(-39293)]

Possibility of having parameterized definitions in haskell?
  Is there a way to compactly write multiple definitions in haskell via case, without having to repeat, other than the input parameters, the exact same syntax?
    Example of defining binary max and min functions.  Can we compress

    max' x y
      | x > y = x
      | otherwise = y

    min' x y
      | x < y = x
      | otherwise = y

into something like

    (max',min') x y
      | x (>,<) y = x
      | otherwise = y


    maxmin x y f
      | f x y = x
      | otherwise = y

    max' x y = maxmin x y (>)
    min' x y = maxmin x y (<)
