:m +Text.Read

:t readMaybe


let a = "3" :: Maybe Integer

let z = readMaybe "3" :: Maybe Integer


:t readMaybe "3" :: Maybe Integer

was misdefining the b as a list

:{
let conc [] ys = ys
    conc a@(x:xs) b = x : conc xs b
:}

conc [] [3]

[(+3) x | x <-[1..4]]

map (+3)

let c = foldl (++) []

c [[2,3],[4,3],[3,4]]


let unt x = x : []

unt 3

con [1,3,2] []
conc [1,2] []

:{
let asdf []     ys = ys
    asdf (x:xs) ys = x : asdf xs ys
:}


[(x,y) | x <- [1..5], y <- [1..5]]

let f x = [x^2| ]

(map (\y -> (y,3)) [1..5])

[x | x <- [1,2,4,5], x <- [1,2,3]]
[x | x <- [1,2,4,5], x <- [1,2,3]]

(&&) 3 4

:t (&&) True False
:t (&&)



data B = F | T deriving (Show,Eq)

:{
let negate :: B -> B
    negate F = T
    negate T = F
:}

:{
let and :: B -> B -> B
    and F _ = F
    and _ F = F
    and T T = T
:}

-- shorter and, order of these lines matter

:{
let and :: B -> B -> B
    and T T = T
    and _ _ = F
:}

let nandor b = (negate .) . b
let nand = nandor and
let nor = nandor or

xor x y =

:{
let or :: B -> B -> B
    or F F = F
    or _ _ = T
:}


[(x,y) | x <- [T,F], y <- [T,F]]

map (\x -> and (fst x) (snd x)) [(x,y) | x <- [T,F], y <- [T,F]]

map (\f -> map (\x -> f (fst x) (snd x)) [(x,y) | x <- [T,F], y <- [T,F]]) [and,or,nand,nor]

let xor x y = and (or x y) (nand x y)
u
let equal = (negate .) . xor

let ex46 x y = and x (or x y)

truthValues ex46

let truthValues f = map (\x -> f (fst x) (snd x)) [(x,y) | x <- [T,F], y <- [T,F]]

map (\x -> equal (fst x) (snd x)) [(x,y) | x <- [T,F], y <- [T,F]]


[(x,y) | x <- [True,False], y <- [True,False]]


[(x,y) | x <- [1..5], y <- [1..5]]


nand T F

let nand = (negate .) . and



-- law number 3 in wadler's paper
c $ map (\x -> (map (\y -> (y,x)) [1..5])) [1..10]

-- here's how to define comprehensions [t | x <- u] === map (\x -> t) u


True || False && True
True && False || True

False || True && False
False && True || False

2 * 3 + 2

2 * 3 + 2
2 + 3 * 2


[x^2| x <- [1,2,3]]
map (^2) [1,2,3]

unwordsList [1,2]

[[x,y,z]|x <- [1,2], y <- [1,2,3], z <- [4,5]]


-- need some kind of monads to really begin writing the complier
-- convertListComp [f | x <- l] = "map"





asdf [1,2] []

plet (asdf) x y = x + y
3 asdf 4

[3] /= []

:t conc

:{
let mp f [] = []
    mp f (x:xs) = f x : mp f xs
:}

mp (+3) [1,2]




