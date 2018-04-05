
-- sqrt fcn

f x n = x^2 - n

x2 x0 = x1

-- Newton's Method

:{
let xn 0 x0 = x0
    xn m x0 = xnm1 - ((xnm1^2) - 612)/ (2* xnm1)
      where xnm1 = xn (m-1) x0
:}

xn 1 10
xn 1 10

map (\x -> xn x 10) [1..5]


nthRoot x n m x0 =

:{
let xn x 0 x0 = x0
    xn x m x0 = xnm1 - ((xnm1^2) - x)/ (2* xnm1)
      where xnm1 = xn x (m-1) x0
:}

:t xn

xn 5.1 2 2.23606

-- :t (^)
-- yields
-- (Eq a, Fractional a1, Num a) => a1 -> a -> a1 -> a1

(Eq a, Fractional a1, Num a) => a -> a1 -> a -> a1 -> a1

:{
let xn :: (Eq a, Fractional a1, Num a) => Integral -> a1 -> a -> a1 -> a1
    xn n x 0 x0 = x0
    xn n x m x0 = xnm1 - ((xnm1^n) - x)/ (n* xnm1)
      where xnm1 = xn n x (m-1) x0
:}


-- finally, *MAKE SURE to be careful of the type signature of mathematical operations
-- (**) was neede instead of (^)

:{
let xn n x 0 x0 = x0
    xn n x m x0 = xnm1 - ((xnm1**n) - x)/ (n* xnm1)
      where xnm1 = xn n x (m-1) x0
:}

:t xn

xn 2 612 3 10

map (\x -> xn 2 612 x 10) [1..5]

-- how to do this s.t. each iteration is cons'ed on a list??


:{
let xn n x 0 x0 = x0
    xn n x m x0 = xnm1 - ((xnm1**n) - x)/ (n* xnm1)
      where xnm1 = xn n x (m-1) x0
:}

let asdf = map (\x -> xn 2 612 x 10) [1..5]


:{
let mapDiff [x] = []
    mapDiff (x:y:xs) = x - y : mapDiff (y:xs)
:}

mapDiff asdf

3 **4

:t (^)
:t (*)

:t xn

xn 2 5 5 2

3.4 ^ 3

:{
let nthRoot n x 0 x0 = x0
    nthRoot n x m x0 = xnm1 - ((xnm1^n) - x)/ (n* xnm1)
      where xnm1 = nthRoot n x (m-1) x0
:}

nthRoot 2 2 2 1

nthRoot 2 612 2 1


x(n+1) = x(n) - f(x(n))/f'(x(n))




:{
let eu :: Double -> Double -> Double -> Double -> Double
    eu x h x0 y0
      | x > x0 = yn + h * ((x-h)^2 - yn^2)
      | otherwise = y0
      where yn = eu (x-h) h x0 y0
:}

eu 1 0.001 0.01 2

:{
let eu :: Double -> Double -> Double -> Double -> (Double -> Double -> Double) -> Double
    eu x h x0 y0 f
      | x > x0 = yn + h * (f (x-h) yn)
      | otherwise = y0
      where yn = eu (x-h) h x0 y0 f
:}

let h y = y

eu 3 1 0 1 h
