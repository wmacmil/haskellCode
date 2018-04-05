
-- the solver

:{
let dsolveBy _ _ [] _ = error "empty solution interval"
    dsolveBy method f mesh x0 = zip mesh results
      where results = scanl (method f) x0 intervals
            intervals = zip mesh (tail mesh)
:}

let intervals x = zip x (tail x)

intervals [1..4]

-- 1-st order Euler
let euler f x (t1,t2) = x + (t2 - t1) * f t1 x

let h x y = y

let sol1 = dsolveBy euler h [0,0.25..4] 1

let sol1 = dsolveBy rk2 h [0,0.25..4] 1

sol1

let sol1 = dsolveBy rk4 h [0,0.25..4] 1

let sol1 = dsolveBy rk2 h [0,0.01..4] 1

let f' x y = x^2 - y^2

let sol2 = dsolveBy euler f' [0,0.1,0.2] 1

sol2

sol1

53.26 - 52.74

-- heun's method

-- 2-nd order Runge-Kutta

:{
let rk2 f x (t1,t2) = x + h * f (t1 + h/2) (x + h/2*f t1 x)
      where h = t2 - t1
:}

-- 4-th order Runge-Kutta

:{
let rk4 f x (t1,t2) = x + h/6 * (k1 + 2*k2 + 2*k3 + k4)
      where k1 = f t1 x
            k2 = f (t1 + h/2) (x + h/2*k1)
            k3 = f (t1 + h/2) (x + h/2*k2)
            k4 = f (t1 + h) (x + h*k3)
            h = t2 - t1
:}

scanl (*) 3 [1..5]

scanl (*) 3 [1..5]
  -- == [3, 3* 1, 2 * 3 , 3 * (2 *3), 4 * (3*6), 5 * 72]

4 * 18

scanl :: (b -> a -> b) -> b -> [a] -> [b]


:{
let scanl f _ [] = []
    scanl f y (x:xs) = f y x : scanl f (f y x) xs
:}

-- How to define scanl without the extra function consing the initial initial y?

:{
let scanl f _ [] = []
    scanl f y (x:xs) = f y x : scanl f (f y x) xs
:}

let scanl' f y (x:xs) = y : scanl f y (x:xs)

-- compressed

:{
let scanl' f y (x:xs) = y : scanl f y (x:xs)
          where scanl f _ [] = []
                scanl f y (x:xs) = f y x : scanl f (f y x) xs
:}

scanl' max 5 [1,2,3,4,5,6,7]

-- This peculiar arrangement is necessary to prevent scanl being rewritten in
-- its own right-hand side.
{-# NOINLINE [1] scanl #-}
scanl                   :: (b -> a -> b) -> b -> [a] -> [b]
scanl                   = scanlGo
  where
    scanlGo           :: (b -> a -> b) -> b -> [a] -> [b]
    scanlGo f q ls    = q : (case ls of
                               []   -> []
                               x:xs -> scanlGo f (f q x) xs)
scanl (\x y -> 2*x + y) 4 [1,2,3]

scanl (*) 3 [1..5]

:{
let scanl f _ [] = []
    scanl f y (x:xs) = y : scanl f y x : scanl f (f y x) xs
:}

scanl (*) 3 [1..5]

scanl :: (b -> a -> b) -> b -> [a] -> [b]

scanl is similar to foldl, but returns a list of successive reduced values from the left:

scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]

Note that

last (scanl f z xs) == foldl f z xs.
