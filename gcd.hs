mod 5 2

mod 351 38

mod 3 1

gcd 50 25

mod 49 14

mod 14 7

:{
let gcd 0 y = y
    gcd x y
      | x < 0 = gcd (-x) y
      | y < 0 = gcd x (-y)
      | x > y = gcd mxy y
      | otherwise = gcd y x
      where mxy = mod x y
:}

gcd 49 21

:{
let coprime x y
      | gcd x y == 1 = True
      | otherwise = False
:}

coprime 41 51
coprime 40 50


-- Eulers' Totient Phi

let phi x = map (coprime x) [1..(x-1)]

phi 5

:{
let countTrue [] n = n
    countTrue (x:xs) n
      | x == True = countTrue xs n+1
      | otherwise = countTrue xs n
:}

-- could just foldr with a lambda istrue?

phi 8

countTrue (phi 8) 0

countTrue (phi 16) 0

countTrue (phi 10) 0

let tot n = countTrue (phi n) $ 0

tot 10

let primeF x y = map (gcd x) [1..y]

primeF 150 150

150 / 6

True (And) False

True && False

not True


not False

:t (&&)

-- doesn't work
:{
let and :: Bool -> Bool -> Bool
    and x y
      | True True   = True
      | True False  = False
      | False True  = False
      | False False = False
:}

:{
let and True True = True
    and _    _    = False
:}

:{
let and True True = True
    and _    _    = False
:}

:{
let or False False = False
    or _     _     = True
:}

and True False

or True (3 == 4)

:{
let not' :: Bool -> Bool
    not' True = False
    not' False = True
:}

not' (3 == 4)

-- how to do this point free?

let nand x y = not $ and x y

let nor x y = not $ or x y

let xand x y = or (and x y) (nor x y)

let xor x y = not $ xand x y

xor True False
xor False True
xor True True
xor False False

let implies a b = or (not a) b

:{
let table :: (Bool -> Bool -> Bool) -> IO ()
    table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b)
                                    | a <- [True, False], b <- [True, False]]
:}

:t or'

table (\a b -> (and a (or a b)))


map (\n -> replicate n 'a') [1..5]

map (\n -> replicate n "ab") [1..5]


fmap (\n -> replicate n 'a') (1 <| 3 <| 5 <| empty)

1 0 0 1

