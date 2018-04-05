
transpose (transpose [[1..4],[4..7]])



tr ((x:xs):xss) = x : tr xss



transpose               :: [[a]] -> [[a]]
transpose []             = []
transpose ([]   : xss)   = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])

let asdf xss = [h | (h:_) <- xss]

:{
let transpose               :: [[a]] -> [[a]]
    transpose []             = []
    transpose ([]   : xss)   = transpose xss
    transpose y@((x:xs):xss) = fdsa y : transpose (xs: [ t | (_:t) <- xss])
:}

(transpose [[1..4],[4..7]])

transpose

:{
let fdsa [] = []
    fdsa (x:xs) = head x : fdsa xs
:}

[x + x | x <- [1..4]]

transpose (transpose [[1..4],[4..7]])

asdf [[1..4],[4..7]]

fdsa [[1..4],[4..7]]

asdf [[1..4]]

transpose (




:{
let unitV _ 0 = []
    unitV (-1) m = 0 : unitV (-1) (m-1)
    unitV 0 m = 1 : unitV (-1) (m-1)
    unitV n m = 0 : unitV (n-1) (m-1)
:}

-- a little cleaner

:{
let unitV _ 0 = []
    unitV 0 m = 0 : unitV (-1) (m-1)
    unitV 1 m = 1 : unitV 0 (m-1)
    unitV n m = 0 : unitV (n-1) (m-1)
:}

-- cleanest, the second line in the above definition is redundant

:{
let unitV _ 0 = []
    unitV 1 m = 1 : unitV 0 (m-1)
    unitV n m = 0 : unitV (n-1) (m-1)
:}

unitV 2 6

unitV 3 5

-- my solution for a 5x5 identity matrix

map (\x -> unitV x 5) [1..5]

let unitMatDimn n = map (\x -> unitV x n) [1..n]

let unitMatDimn2 n = map (flip unitV n) [1..n]

unitMatDimn2 3
unitMatDimn2 8

:t flip


unitMatDimn 3
unitMatDimn 8



let matI n = [ [fromEnum $ i == j | i <- [1..n]] | j <- [1..n]]

fromEnum 'a'

map fromEnum "asdf"

showMat $ matI 3

:{
let showMat :: [[Int]] -> String
    showMat = unlines . map (unwords . map show)
:}

