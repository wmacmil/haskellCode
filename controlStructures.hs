
:{
let f 0 = 18
    f 1 = 15
    f 2 = 12
    f x = 12 - x
:}


map f [1..10]

let f = (+1)

:{
let f x =
        case x of
            0 -> 18
            1 -> 15
            2 -> 12
            _ -> 12 - x
:}


:{
let f x
      | x == 0 = 18
      | otherwise = 12 - x
:}

