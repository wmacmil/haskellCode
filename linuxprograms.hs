compareTwoLists (x:xs) (y:ys)

null

:{
let isInList x [] = False
    isInList x (y:ys)
      | x == y = True
      | otherwise = isInList x ys
:}

isInList 3 [1..5]

:{
let pair x y = zipWith pairElem x y
      where pairElem a b = (a,b)
:}

pair [1..5] [6..11]

pair [1..5] ['a'..'e']

:{
let iasdf x y = pair x z
      where z = map (\x -> isInList x y) x
:}

iasdf [1..5] [1..10]
iasdf [1..15] ([1..10] ++ [22,12,13,35,14])

:{
let sndTrue [] = []
    sndTrue (x:xs)
      | snd x == True = fst x : sndTrue xs
      | otherwise = sndTrue xs
:}

sndTrue $ iasdf [1..15] ([1..10] ++ [22,12,13,35,14])

intersect xx@(x:xs) yy@(y:ys) = map (int xx) yy
  where xx x ys =








