
-- three types of operations
-- swap
-- multiply by scalar
-- add one row to another


let x1 = [3,4,5]
let x2 = [2,1,4]

head x1 / head x2 :: Rational

:t x1

:{
let asdf :: [Int]
    asdf = [1..3]
:}

:t asdf

-- reduce row, not getting it to work with the right type signature

:{
let rr :: Int a => [a] -> [a] -> [Rational]
    rr x1 x2 = zipWith (-) x1 mb
      where mb = map (*(head x1 / head x2)) x2
:}

:{
let rr x1 x2 = zipWith (+) mb x2
      where mb = map (*gk) x1
            gk = -(head x2 / head x1)
:}

let y = rr a b
let z = rr a c

rr (tail y) (tail z)

:{
let ut xs = lg : map (rr lg) gj
      where lg = head xs
            gj = tail xs
:}

-- need an extra tail somewhere!
--so go ut x -> ut map tail (tail ut)
-- works
-- how to account for the zero padding
--
ut [a,b,c]

let mt = map tail

ut $ mt $ tail $ ut [a,b,c]

let dunno x = ut $ mt $ tail $ ut x

dunno [a,b,c]

a : (map (0:) (dunno [a,b,c]))

fjk x n =

-- need to somehow recursively do all of this

a



let a = [2,1,-1,8]
let b = [-3,-1,2,-11]
let c = [-2,1,2,-3]


