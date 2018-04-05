Circle :: Float -> Float -> Float -> Shape

data Shape = Circle Float Float Float | Rectangle Float Float Float Float

:t Circle

:t Shape

:{
let surface :: Shape -> Float
    surface (Circle _ _ r) = pi * r ^ 2
    surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
:}

let x = Shape Circle 3 3 3

data Person = Person String String Int Float String String deriving (Show)

let asdf = Person "asdf" "asfd" 3 2.3 "asdf" "asdf"

asdf

:{
data Person = Person { firstName :: String
                         , lastName :: String
                         , age :: Int
                         , height :: Float
                         , phoneNumber :: String
                         , flavor :: String
                         } deriving (Show)
:}


let guy = Person "Budsy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

firstName guy


let myC x y z

let x = Circle 3 4 5

surface x

data Vector a = Vector a a a deriving (Show)

let x = Vector 3 9 4

x

:{
let vplus :: (Num t) => Vector t -> Vector t -> Vector t
    vplus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)
:}

data V a = V [a] deriving (Show)


let vplus :: (Num t) => Vector t -> Vector t -> Vector t

let mp = map (+)

:{
let vP :: (Num t) => V t -> V t -> V t
    vP (V [x]) (V [y]) = V (mp [x] [y])
:}

-- only works for even lists s.t. lenght(list_) is even
--modified with two base conditions, i think this now works for everything

:{
let evens [] = []
    evens [x] = [x]
    evens (x:y:xs) = x : evens xs
:}

evens [1..20]

evens [1..21]


evens [1..4]
evens [1..5]

evens [1,2,3,4234,343,2,1]
odds [1,2,3,4234,343,2,1]


-- inverse function if we apply both evens and odds to the same input
-- e.g. recombine $ (odds $ l) (evens $ l) = identity
-- still need to fix conditions for odd input

:{
let recombine [] []  = []
    recombine [] [x] = [x]
    recombine [x] [] = [x]
    recombine (x:xs) (y:ys) = x : y : recombine xs ys
:}

let as = evens [1,2,3,4234,343,2]
as

let sa = odds [1,2,3,4234,343,2]
sa

recombine (evens [1,2,3,4234,343,2]) (odds [1,2,3,4234,343,2])

recombine (evens [1,2,3,4234,343,2,2]) (odds [1,2,3,4234,343,2,2])

recombine . evens $ [1,2,3,4234,343,2] . odds $ [1,2,3,4234,343,2]

-- testing function
-- how to do this without parens?
let recEvOdd x = recombine (evens x) (odds x)
recEvOdd [1..100]

-- testing function
-- how to do this without parens?

let secEvOdd x = recombine . evens x odds . x

evensAndOdds x = evens x

let foobar' x = (`map` [id, reverse]) ($ x)

let foobar x = [id x, reverse x]

foobar [1..23]

id [1..2]

recEvOdd [1..100]



let a = (True && False)
let b = (True || False)
a
b

evens [1..21]
evens [1..20]


evens [1..4]
evens [1..5]


let odds (x:xs) = evens xs

odds [1..21]
odds [1..20]


vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n  1


