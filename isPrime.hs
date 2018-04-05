
isPrime x
  | x / i ==


primes n = [1..

gcd a b =

:{
let modulo x y
      | n < 0 = x
      | otherwise = modulo n y
      where n = x - y
:}

map (modulo 63) [1..33]

map (mod 63) [1..33]

round (.5)

:t rndRoot

let rndRoot = round . sqrt

:{
let rndRoot :: (Floating a, Integral c, RealFrac a) => a -> c
    rndRoot = round . sqrt
:}

:{
let oneToRndRoot :: (Floating a, Integral t, RealFrac a) => a -> [t]
    oneToRndRoot x = [1..rndRoot(x)]
:}

:t oneToRndRoot

let mapMod x = map (modulo x)

mapMod 49 (oneToRndRoot 49)

mapMod 49 $ oneToRndRoot 49

let mapModToRndRoot x = mapMod x $ oneToRndRoot x

mapMod 39 $ oneToRndRoot 39

mapModToRndRoot 39

map $ (modulo 800) (oneToRndRoot 800)

map (modulo 800) (oneToRndRoot 800)

let mapModToRndRoot2 x y = map (modulo x) (oneToRndRoot y)

mapModToRndRoot2 33 33


let isP x = asdf x (x + 1 -1)

isP 303

asdf 33 33

rndRoot (33)


let isP x = map (modulo x) $ (oneToRndRoot x)

isP 800

    -- Idea is to create a list of N modulo {all integers up to a rounded sqrt (N)}, and then test the resulting list for a zero other than the initial index.  The first four functions all work.

-- wrong answer
    :{
    let rndRoot :: (Floating a, Integral c, RealFrac a) => a -> c
        rndRoot = round . sqrt . fromIntegral
    :}

:{
let rndRoot :: (Integral a) => a -> a
    rndRoot = round . sqrt . fromIntegral
:}

:{
let oneToRndRoot :: (Integral a) => a -> [a]
    oneToRndRoot x = [1..rndRoot(x)]
:}

:{
let modulo x y
      | n < 0 = x
      | otherwise = modulo n y
      where n = x - y
:}

let mapMod x = map (modulo x)

mapMod 49 (oneToRndRoot 49)

    -- however, while the repl accepts this without complaint, it spits out the following error message

let mapModToRndRoot x = mapMod x $ oneToRndRoot x

mapModToRndRoot 39

    <interactive>:475:1:
        No instance for (Floating a0) arising from a use of ‘it’
        The type variable ‘a0’ is ambiguous
        Note: there are several potential instances:
          instance Floating Double -- Defined in ‘GHC.Float’
          instance Floating Float -- Defined in ‘GHC.Float’
        In the first argument of ‘print’, namely ‘it’
        In a stmt of an interactive GHCi command: print it

    -- The ad hoc solution which seems to work fine is just to use two arguments rather than repeat the same one

    let mapModToRndRoot2 x y = map (modulo x) (oneToRndRoot y)

    Prelude> mapModToRndRoot2 33 33
    [0,1,0,1,3,3]


