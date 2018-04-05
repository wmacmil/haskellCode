
:{
let powerset :: [a] -> [[a]]
    powerset [] = [[]]
    powerset (x:xs) = xss ++ map (x:) xss
                    where xss = powerset xs
:}

Elegant as it is, this program causes a serious space leak.
(In fact, it is often cited as an example of why functional
programming language implementations might choose not to use
common subexpression elimination.)

Why?  Because it holds on to the whole of xss until the first
half of the resulting list has been generated.  And xss gets
big, fast.  In Hugs, try:

   :set +g
   length (powerset [1..32])

and watch as your free heap disappears ...

One way to fix this is to rewrite the second line in the
definition of powerset as:

   powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

Or, if duplicated computation offends you, replace (++) in the
original version of powerset with an interleave operator:

   powerset       :: [a] -> [[a]]
   powerset []     = [[]]
   powerset (x:xs) = xss /\/ map (x:) xss
                    where xss = powerset xs

   (/\/)        :: [a] -> [a] -> [a]
   []     /\/ ys = ys
   (x:xs) /\/ ys = x : (ys /\/ xs)

These two variants both run in constant space (assuming that
your compiler isn't "smart" enough to do common subexpr
elimination :-)

:{
let fmap' f :: [a] -> [b]
    fmap' f xs = [ f a | a <- xs ]
:}


fmap [1..4]


return :: T -> [T]
return x = [x]

join :: [[T]] -> [T]
join xs = concat xs


let ps1 x:y = [[x]]


type Set a = [a]

:{
let powerset :: Set a -> Set (Set a)
    powerset [] = [[]]
    powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs
:}

powerset [1..4]


-- list comprehensions

[x^2 | x <- [1..4]]

[x^2 | [1..4] -> x]

-- doesn't work easily for dimension > 2
[[x,y,z] | x <- [0,1], y <- [0,1], z <- [0,1], x /= y || y /= z]

[[x,y,z] | x <- [0,1], y <- [0,1], z <- [0,1], x /= y || y /= z]

:{
let evens [] = []
    evens [x] = [x]
    evens (x:y:xs) = x : evens xs
:}

:{
let odds [] = []
    odds (x:xs) = evens xs
:}


let oddsAndEvens a@(x:y:xs) = (evens a,odds a)

oAEs a@(x:y:xs) = (x: oAEs xs, y: oAEs xs)

oddsAndEvens [1..10]
oddsAndEvens [1..11]

odds [1..6]
odds [1..7]

evens [1..6]
evens [1..7]

oAE (x:y:xs) = (x:[x1)],[x2])
  where x1 =

[([x1],[x2]) | x1 <-

