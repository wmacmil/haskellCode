
:{
let token k xs
      | k == take n xs = [(drop n xs,k)]
      | otherwise = []
      where n = length k
:}

:t token
:t satisfy

:{
let satisfy p [] = []
    satisfy p (x:xs) = [ (xs,x) | x <- p]
:}


token "asdf" "asdfjdjs"
token "asdf" "basdfjdjs"



:t token
