

:{
intersperse c [x] = [x]
intersperse c (x:xs) = x : c : intersperse c xs
:}

intersperse 3 [1..5]

:t intersperse

:{
intersperse c [x] = [x]
intersperse c (x:xs) = (:) x ((:) c (intersperse c xs))
:}

intersperse 3 [1..5]

:{
inters c f [x] = [x]
inters c f (x:xs) = f x (f c (inters c xs))
:}


[1..5] ++ [3..15]

:{
intercalate :: [a] -> [[a]] -> [a]
intercalate c [x] = x
intercalate c (x:xs) = x ++ c ++ intercalate c xs
:}

intercalate [11..14] (map (\n -> [1..n]) [1..3])

let asdf = map (\n -> [1..n]) [3,3,3]


nemptys = ncons [] (length y)

ncons [] 4

:{
ncons c 0 = []
ncons c n = c : ncons c (n-1)
:}

-- -- redundant
-- :{
-- transpose a = []
-- transpose y@(x:xs) = map head y : transpose (map tail y)
--   where a = ncons [] l
--         l = length y
-- :}

-- worked it out here
:{
transpose [] = []
transpose y@(x:xs)
  | y == a = []
  | otherwise = map head y : transpose (map tail y)
  where a = ncons [] l
        l = length y
:}

transpose [[1,3,2],[2,4,3],[5,3,4]]

:{
pp [] [] = []
pp [] (y:ys) = y : pp [] ys
pp (x:xs) ys = x : pp xs ys
:}

pp [3..5] [1..4]
pp [] [1..4]
pp [1..4] []
pp [] []

-- quick and dirty concat
cc xs = foldr (++) [] xs

cc [[3,4,5],[2,3,4],[2,1,1]]

:{
concat [] = []
concat (x:xs) = pp x (concat xs)
:}

concat [[3,4,5],[2,3,4],[2,1,1]]

concatMap f xs = concat $ map f xs

concatMap (replicate 3) [[3,4,5],[2,3,4],[2,1,1]]

and = foldr (&&) True
or = foldr (||) False

or $ map (==4) [2,3,4,5,6,1]

:t or

any f xs = or $ map f xs

any = (or .) . map
all = (and .) . map

fj = (+3)

fj . fj $ 3

-- iterate
iter f x = f x : iter (f . f) x

take 10 $ iter (+3) 1



length $ transpose [[2,3],[5,4]]

transpose [[1,3,2],[2,4,3],[5,3,4]]



