
:{
let ia x xs c = (take b xs) ++ [x] ++ (drop b xs)
      where b = c-1
:}

:t ia

ia 'x' "asdf" 2
ia 3 [1,2,3,4] 2


e.g. write
range 4 9

:{
let range x y
      | x <= y = x : range (x+1) y
      | otherwise = []
:}
range 4 9



