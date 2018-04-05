
-- deriv of poly
-- d/dx 3x^2 - 4x + 5 = 3 * 2 x - 4
--

[5,-4,3] = [-4,3*2]

let dpolydx n (x:xs) = zipWith (*) [1..n] xs

dpolydx 2 [5,-4,3]

-- how to do this without explicitly using an n paremeter?
--

map (^3) [1..4]

-- how to generate a list of fractions from 0 to 1

:{
let nMs 0 m = []
    nMs n m = m : nMs (n-1) m
:}

let uio = nMs 39 2

length([1..4])
length(uio)

nMs 3 2

zipWith (/) uio [1..33]


let nMFracs n m = zipWith (/) (nMs n m) [1..n]

nMFracs 10 1

map (/100) [1..100]

map (/100) [1..100]

-- how to create a function to generate a list from a to b divided n times
-- maybe try n+1?

2 4 10

let bjkc b a n = map (/(n/(b-a))) [0..n]

:{
let bj b a n = map (/c) [0..n]
      where c = n/(b-a)
:}

bj 2 4 10
bj 4 1.5 10

let aToBNPlus1Times b a n = map (+ a) $ bj b a n

length $ aToBNPlus1Times 12 (-12) 24



aToBNtimes 12 (-12) 24

aToBNtimes (12) (0.13) 24

map (^2) $ aToBNtimes (11) (6) 24

map (p 2) $ aToBNtimes (11) (6) 24

-- finally correct.  make sure to be careful about numerics from now on
map ((**) 2) $ aToBNtimes (11) (6) 24

map (2**) $ aToBNtimes (11) (6) 24

map (**2) $ aToBNtimes (11) (6) 24

2 ^ 11

let twoTwoThe x = (^) 2 x

twoTwoThe 3

map twoTwoThe [1..4]

-- ahh now i get where the error is coming from
twoTwoThe (1.1)

map twoTwoThe $ aToBNtimes (11) (6) 24

-- https://stackoverflow.com/questions/4553405/how-can-i-bind-the-second-argument-in-a-function-but-not-the-first-in-an-elegan

map (flip (^) 2) $ aToBNtimes (11) (6) 24

map (p 2) $ aToBNtimes (12) (0.13) 24

(^) 3 4

let p x y = (^) y x

-- remember to put a zero before a decimal
(-1.12) + (0.12)


let sub = (-)

map (`sub` 5) [1..5]


primeFactors n =

-- any number x = my + r

mod 67 46

mod 46 21

mod 21 4

mod 12 6

mod 1 1

:{
let gcd x 0 = x
    gcd x y = gcd y (mod x y)
:}

:t gcd


gcd 78 39

gcd 4 2

gcd 21 42
gcd 21 42

gcd 4 6

gcd 6 9

gcd 4 9

-- how to get the gcd of a list of numbers?

mod 7 5

let lcm x y = (x * y) / (gcd x y)

lcm 3 0
lcm 0 3

let lcm x y = (x * y) / (gcd x y)

let lcm x y = quot (x * y) (gcd x y)

-- doesn't work because (/) operates over fractionals

:t quot
:t (/)

:t gcd

33.1 / 12.3

lcm 83 23

lcm 8 4

lcm x y
  | (mod x y) == 1


mod 8 6

lcm 6 8 == lcm (2*3) (2*2*2) = 2 * lcm 3 (2 * 2)

lcm 3 4 = 12

-- how to compute this?



take' 2 [1..4]

:{
let take' _ [] = []
    take' 0 _ = []
    take' n (x:xs) = x : take' (n-1) xs
:}

take' 3 [1..5]

take' 3 []

:{
let filt f [] = []
    filt f (x:xs)
      | f x = x : filt f xs
      | otherwise = filt f xs
:}

filt (< 45) [40..70] ++ [1]

-- could use primes up to n-1 to save space in the below algorithm

:{
let prime_factors :: Int -> [Int]
    prime_factors 1 = []
    prime_factors n
      | factors == []  = [n]
      | otherwise = factors ++ prime_factors (div n (head factors))
      where factors = take 1 $ filter (\x -> (mod n x) == 0) [2 .. n-1]
:}

take 1 []

head 1 []



prime_factors 120

-- 120 -> 60 -> 30 -> 15 -> 5 -> return [5]

map prime_factors [1..100]

map (\x -> (^) x 3) [1..10]

take 1 $ filter (\x -> mod x 7 == 0) [2..10]

filter (\x -> mod x 3 == 0) [1..10]




prime_factors n =
  case factors of
    [] -> [n]
    _  -> factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]




:{
let newtonRoot
      :: (Floating a, Ord a)
      => Natural  -- ^ iterations
      -> a        -- ^ epsilon
      -> a        -- ^ starting guess
      -> (forall b. Floating b => b -> b)
      -> Maybe a
    newtonRoot i ep x f
       | i == 0 = Nothing
       | abs (f x) - abs ep < 0 = Just x
       | otherwise = newtonRoot (i - 1) ep (x - f x / diff f x) f
:}


import Numeric.Natural (Natural)

import Numeric.AD (diff)

:{
let newtonRoot i ep x f
      | i == 0 = Nothing
      | abs (f x) - abs ep < 0 = Just x
      | otherwise = newtonRoot (i - 1) ep (x - f x / diff f x) f
:}


