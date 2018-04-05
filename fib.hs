

:{
let fib 0 = 1
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)
:}


fb n =

map (map (*3)) [[1,0],[0,1]]

-- tensor scalar multiplicaton

map (map (map (*3))) [[[1,0],[0,1]],[[1,0],[0,1]]]

zipWith (zipWith (+)) [[1,0],[0,1]] [[1,0],[0,1]]


-- how to compute paritions?
p 3 = [[1,1,1],[2,1]]

p 3 = [[1,1,1],[2,1]]

p 4 = [[1,1,1,1],[2,1,1],[2,2]]



span' f (x:xs) = let (ys,zs) = span' f xs in (x:



pack (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack rest
pack [] = []






newtype Matrix a = Matrix [[a]] deriving (Eq, Show)

:{
instance Num a => Num (Matrix a) where
    Matrix as + Matrix bs = Matrix (zipWith (zipWith (+)) as bs)
    Matrix as - Matrix bs = Matrix (zipWith (zipWith (-)) as bs)
    Matrix as * Matrix bs =
       Matrix [[sum $ zipWith (*) a b | b <- transpose bs] | a <- as]
    negate (Matrix as) = Matrix (map (map negate) as)
    fromInteger x = Matrix (iterate (0:) (fromInteger x : repeat 0))
    abs m = m
    signum _ = 1
:}

let fib n = head (apply (Matrix [[0,1], [1,1]] ^ n) [0,1])


