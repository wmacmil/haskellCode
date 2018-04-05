
:{
let derivative :: (Fractional a) => a -> (a -> a) -> (a -> a)
    derivative h f x = (f (x + h) - f (x)) / h
:}

let h x = x^3

h 4

derivative 0.1 h 3

let h x = sin x

-- to see how derivative changes with different h values
map (\x -> derivative x h 3) b

b

a

-- 1 1/10 1/100

let b =scanl (*) (1/10) a


:{
let nms 0 _ = []
    nms n m = m : nms (n-1) m
:}

let a = 1 : nms 4 0.1

-- upIntegral f a b h = sum f (a + h) * h

:{
let evalPts a b h
      | a < b = a : evalPts (a+h) b h
      | otherwise = []
:}

evalPts 3 4 0.1

-- drop 1 $ revalPts 3 4 0.1

-- Here's the easiest option

let xas = tail $ revalPts 3 4 0.1

let ff x = x^3 + x^2 -3*x + 5

map ff xas

xas

-- xs Is the set of pts f is evaluated at
-- ys is the function evaluated at those poitns

:{
let upInt f a b h = sum $ zipWith (*) xs ys
      where xs = tail $ revalPts a b h
            ys = map f xs
:}

:{
let upInt f a b h = sum $ map (*h) ys
      where xs = tail $ revalPts a b h
            ys = map f xs
:}

upInt ff 0 1 0.01

map (upInt ff 0 1) b

upInt (\x -> x^4 *5) 0 2 0.01

map (upInt (\x -> x^4 *5) 0 2) b

2 ^ 3 * 5

Just 2

map Just [1..4]

[1,"a"]

b

upIntegral f a b h =


init $ revalPts 3 4 0.1

:{
let revalPts a b h
      | a < b = a : revalPts (a+h) b h
      | otherwise = [b]
:}



:{
let levalPts a b h
      | b - (a+h) > (h^2)  = a : levalPts (a+h) b h
      | otherwise = [b]
:}

evalPts 3 4 0.1

levalPts 3 4 0.1

:{
let levalPts [x] = []
    levalPts (x:xs) = x : levalPts xs
:}

let leval = levalPts evalPts

lowIntegral f a b h



