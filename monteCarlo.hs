import System.Random (randomRIO)

import System.Random

randomRIO' a b =

let newRand = randomIO :: IO Int

(+3) newRand

let a = newRand

let x b = newRand +  b

(+) a 3

mod $ newRand 6


add two random numbers in different ranges

:{
let randomList ::  IO Int
    randomList = do
      r  <- randomRIO (1,6)
      rs <- randomRIO (6,19)
      return (r + rs)
:}

:t randomRIO (3,3)

:{
let x :: IO Int
    x = 3 :: IO
:}

:{
let x :: IO ()
    x = putStrLn "3"
:}

:t randomRIO (1,4)

x

:{
let randomList ::  IO Int
    randomList x = do
      r  <- randomRIO (1,6)
      rs <- x
      return (r + rs)
:}

let hel = putStrLn "Hello" >> putStrLn "World" >> getLine >>= \n -> putStrLn ("hasdfj" ++ n ++ "asdf")

hel


:{
let randomList' ::  IO Int
    randomList' = do
      r  <- randomRIO (1,6)
      rs <- 3
      return (r + rs)
:}

:{
let randomList' ::  IO Int
    randomList' = do
      r  <- randomRIO (1,6)
      rs <- randomRIO (3,3)
      return (r + rs)
:}

randomList'


:{
let randomList :: Int -> IO([Int])
    randomList 0 = return []
    randomList n = do
      r  <- randomRIO (1,6)
      rs <- randomList (n-1)
      return (r:rs)
:}

map (+3) . (randomList 30)



randomList 30

addRand x = do
  g <- randomIO
  g2 <- g + x
  print g2


addRand x = do
  g <- randomIO
  print g + x


3 % 2

:{
let mod x y
      | x > y     = mod (x-y) y
      | otherwise = x
:}

mod 3 2

:{
let randomList :: Int -> IO([Int])
    randomList 0 = return []
    randomList n = do
      r  <- randomRIO (1,6)
      rs <- randomList (n-1)
      return (r:rs)
:}

randomList 38

:{
let randomList :: Int -> Int -> Int -> IO([Int])
    randomList 0 a b = return []
    randomList n a b = do
      r  <- randomRIO (a,b)
      rs <- randomList (n-1) a b
      return (r:rs)
:}


print 3
:t print

print "asdf"
:t show

show 3

show 'a'

show "asdf"

let msh b =  " ++ b ++ "

msh 3

roundTo

(fromInteger $ round $ f * (10^n)) / (10.0^^n)

let fI f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

fI 303.3389383 3
fI 303.3389383 4

let fI f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

:t printf

3 ^^ (-3)

fI 303.3389383 (-1)

fromInteger (-33333982349)

:t fromInteger
:t round


randomList 44 20 30

:{
let randomList :: Int -> [Double]
    randomList seed = randoms (mkStdGen seed) :: [Double]
:}

:t randomList


randomList (-958036805781772734)

take 10 $ randomList (-958036805781772734)


