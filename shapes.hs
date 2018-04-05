data Point = Point { ptX, ptY :: Float}

:{
let x :: Point
    x = (0,1)
:}

let x = Point 0 1
let y = Point 1 1

let l1 = Line x y

l1

data Line = Line { l0, l1 :: Point}
