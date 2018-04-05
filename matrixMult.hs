let zWMult = zipWith (*)

let fPl0 = foldl (+) 0

fp0

let dPr x y = fp0 (dP x y)

dPr [1..5] [2..6]

let dPr x y = fp0 $ dP x y

let dP = foldl $ (+) $ 0 $ zipWith $ (*)

foldl (+) 0 [1..5]

dP [1..5] [2..6]

:{
let zW f [] [] = []
    zW f (x:xs) (y:ys) = f x y : zW f xs ys
:}

:t zW

:{
let zWMult :: Num c => [c] -> [c] -> [c]
    zWMult = zipWith (*)
:}

:{
let foldPl0 :: Num c => [c] -> c
    foldPl0 = foldl (+) 0
:}

let dotPr1 = fZW (+) 0 (*)
let dotPr = fZW (+) 0 (*)

let fZW f i g x y = foldl f i (zipWith g x y)

let fZW1 f i g x = foldl f i . (zipWith g x)

let fZW2 f i g = (foldl f i .) . zipWith g

let dotPr2 = fZW2 (+) 0 (*)

-- does not work with fractionals
-- how to do this with the matrix as the mapped thing?

let mm x y = map (dotPr2 x) y

-- is there a general name for this? it seems like a zipwith except the second list is constantly applied

let nn x y = mm y x

let mam x y = map (nn x) y

-- somehow the transpose

mam [[2,1],[-1,-2]] [[1,0],[1,1]]

mam [[1,0],[2,1],[-1,-2]] [[1,0],[1,1]]

mmm [[1,0],[2,1],[-1,-2]] [[1,0],[1,1]]

:{
let mmm [] _ = []
    mmm (x:xs) y = mm x y : mmm xs y
:}

let zw f [] [] = []
    zw f (x:xs) (y:ys) = f x y : zw f x


mmm [[2,1],[-1,-2]] [[1,0],[1,1]]


let mmm x y z = map

mm [1,0] [1,1]

-- want to make it so that this is not well typed
mm [2,2,1] [[1,0],[1,1],[-1,-2]]

mm [2,1] [[1,0],[1,1],[-1,-2]]

mm [2,1] [[1,0],[1,1],[-1,-2]]




-- example to indicate paramatricity/polymorphism/naturality w.r.t operations f, g

let asdf = fZW2 (++) [] (++)

asdf ["asdsdfjfjdsksdkjbwfasdf","a3kwejrsdf"] ["asdf","asdf"]


dotPr2 [1,0] [1,1]
dotPr2 [1,0] [0,1]







dotPr [1,0] [1,1]
dotPr [1,0] [0,1]

let fZW




:{
let dPr1 :: Int c => [c] -> [c] -> c
    dPr1 = foldPl0 $ zWMult
:}

dPr1 :: Num c => [c] -> [c] -> c
dPr1 = foldPl0 $ zWMult

dPr1 :: Int c => [c] -> [c] -> c
dPr1 = foldPl0 $ zWMult

let dt = (foldPl0 .) . zWMult

-- fold . zipWith
--



let dot = (sum.) . zipWith (*)

dot [1..5] [1..5]





How to define dot product in a pointfree style?  Really, I'd like to figure out the more general solution of composing a fold with a zipwith in a single function point free.

    zWMult :: Num c => [c] -> [c] -> [c]
    zWMult = zipWith (*)

    foldPl0 :: Num c => [c] -> c
    foldPl0 = foldl (+) 0

I get the correct solution when I use arguements

    dPr x y = foldPl0 (zWMult x y)
    dPr x y = foldPl0 $ zWMult x y

But have no idea how to naturally compose these without arguements.  Both these fail:

Prelude> :{
Prelude| let dPr1 :: Num c => [c] -> [c] -> c
Prelude|     dPr1 = fPl0 $ zWMult
Prelude| :}

<interactive>:171:19:
    Couldn't match expected type ‘[[c] -> [c] -> c]’
                with actual type ‘[Integer] -> [Integer] -> [Integer]’
    Relevant bindings include
      dPr1 :: [c] -> [c] -> c (bound at <interactive>:171:5)
    Probable cause: ‘zWMult’ is applied to too few arguments
    In the second argument of ‘($)’, namely ‘zWMult’
    In the expression: fPl0 $ zWMult

Prelude> :{
Prelude| let dPr1 :: Int c => [c] -> [c] -> c
Prelude|     dPr1 = foldPl0 $ zWMult
Prelude| :}

<interactive>:11:13:
    ‘Int’ is applied to too many type arguments
    In the type signature for ‘dPr1’: dPr1 :: Int c => [c] -> [c] -> c

As well as

    dPr2 = foldPl0 . zWMult
