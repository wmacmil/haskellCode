

:{
let splitAt 0 y@(x:xs) = ([],y)
    splitAt n (x:xs) = let (ys,zs) = splitAt (n-1) xs in (x:ys,zs)
:}

splitAt 3 [1..5]

group "mississippi"

groupa "mississippi"

groupa [1,1,2,2,3,2,2,3,3]

:{
let groupa                   :: Eq a => [a] -> [[a]]
    groupa                  =  groupBy (==)
:}

-- | The 'groupBy' function is the non-overloaded version of 'group'.

:{
let groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
    groupBy _  []           =  []
    groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
                               where (ys,zs) = span (eq x) xs
:}

[0.01..0.1]

[0.02..0.2]

module Main where

--import Graphics.Gnuplot.Simple

let dt  = 0.01  -- Timestep

let tau = 0.05  -- Decay constant

let x = [0.0, dt..tau*4]

[0.0, 0.001..1]

[1, 4..98]

[3,6..99]

x

let dt  = 0.01  -- Timestep
let tau = 0.05  -- Decay constant
let n0  = 1000  -- Initial fber of particles


main = do
    --plotList [] list
    print list

f .01

:{
let list :: [(Double, Double)]
    list = [(f t, t) | t <- [0.0, dt..tau*4]]
:}


f :: Double -> Double
f t
    | t > 0     = f (t - dt) - dt / tau * f (t - dt)
    | otherwise = n0


let dt  = 0.01  -- Timestep
let tau = 0.05  -- Decay constant
let n0  = 1000  -- Initial fber of particles

:{
let f t
        | t > 0     = let t' = f (t - dt) in t' - dt / tau * t'
        | otherwise = n0
:}

import Graphics.Gnuplot.Simple


-- eulers method

-- y(n+1) = y(n) + h * f'(x(n),y(n))
--
-- Proper defn.
-- y(n+1) = y(n) + h * A(n)
-- x(n+1) = h + x(n)
-- A(n) = f(x(n),y(n))
--
--ex(1)
--y' = x^2 - y^2
--s.t. y(0) = 1

let h = 0.1

:{
let xn 0 = 0
    xn n = h + xn (n-1)
:}

let an x y = x^2 - y^2

an 0 1 == -1

let y1 = 1 + 0.1 * (-1)
y1 == 0.9

0.1 * (-1)

let eee = reverse [0..10]

map xn eee

:{
let h :: Double
    h = 0.1
:}

:{
let h :: Double
    h = 0.0001
:}

:{
let eu :: Double -> Double
    eu x
      | x > 0 = yn + h * ((x-h)^2 - yn^2)
      | otherwise = 1
      where yn = eu (x-h)
:}

:{
let eu :: Double -> Double -> Double
    eu x h
      | x > 0 = yn + h * ((x-h)^2 - yn^2)
      | otherwise = 1
      where yn = eu (x-h) h
:}

:t eu

1.0 + 0.1 * (- 1.0^2)

eu 1 0.1

map (eu 1) [0.1,0.01,0.001,0.0001,0.00001,0.000001,0.0000001]

eu 0.2
eu 0.1
eu 0


-- with initial conditions xo, yo

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

let f' x y = x^2 - y^2

eu 0.2 0.1 0 1 f'

let f x y = x + y
let g x = x *3


3 * (f 3 4)

g (f 3 4)


-- with initial conditions,trying to generalize
:{
let eu :: Double -> Double -> Double -> Double -> Double
    eu x h x0 y0 f
      | x > x0 = yn + h * an
      | otherwise = y0
      where an = f (x-h) yn
            yn = eu (x-h) h x0 y0 f
:}

let f x y = x^2 + y^2


            an = ((x-h)^2 - yn^2)

-- here's how i figure it must evaluate on 0.1

eu 0.1
-> eu (0.1 - 0.1) + 0.1 * (0.1^2 - eu (0.1 - 0.1) ^2)
-> eu 0 + 0.1 * (0.01 - eu 0 ^ 2)
-> 1 + 0.1 * (0.01 - 1 ^ 2)
-> 1 + 0.1 * (-0.99)

1 + 0.1 * (-0.99)
-- aha! my own attempt at evaluation yields an error in the agorithm
-- needed to have (x-h) instead of x!


:t eu

1.0 + 0.1 * (- 1.0^2)

eu 0.2
eu 0.1
eu 0


:{
let eu :: Double ->
let eu x
      | x > 0 = yn + h * (x^2 - yn^2)
      | x == 0 = 1
      where yn = eu (x-h)
:}


0.9^2

0.01 - 0.81


:{
let a x
      | x > 0 = 3
      | x == 0 = 4
:}
a 3
a 0

:{
let euler n 0 = 1
    euler n x = yn + h * ((x+h)^2 - yn^2)
      where yn = euler (n-1)
:}

euler 2

let A x y = x^2 + y^2


--
-- generally, choose h = delta_h small
-- t(n) is deltah +...+ delta H    n times
--
-- ex:
-- y' = y
-- y(0) = 1
-- => f(t,y) = y' = y
-- => f(0,1) = 1
--


-- eulerstep :: Num t => ((t, t) -> t) -> t -> (t, t) -> (t, t)
eulerStep f step (x, y)= (xnew, ynew)
                    where
                    xnew = x + step
                    ynew = y + step * (f (x, y))


--euler :: (Floating t, Ord t) => ((t, t) -> t) -> t -> t -> t -> t -> [(t, t)]
euler :: ((Double, Double) -> Double) -> Double -> Double -> Double -> Double -> [(Double, Double)]
euler f x0 xf y0 step = xypairs
                     where
                     iterator = iterate $ eulerStep f step
                     xypairs = takeWhile (\(x, y) -> x <= xf ) $ iterator (x0, y0)



groupBy (<) [1,1,2,2,3,2,2,3,3]

groupBy (==) [1,1,2,2,3,2,2,3,3]

groupBy (<3) [1


:{
let pack :: Eq a => [a] -> [[a]]
    pack [] = []
    pack (x:xs) = (x:reps) : (pack rest)
        where
            (reps, rest) = maybe (xs,[]) (\i -> splitAt i xs)
                             (findIndex (/=x) xs)
:}

pack "mississippi"

:{
let grp [] = []
    grp (x:xs) = (x:(filter (==x) xs)):(grp $ filter (/=x) xs)
:}

grp "mississippi"
