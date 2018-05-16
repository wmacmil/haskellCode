

data T a = L a | B a (T a) (T a) deriving (Show)


let t1 = (L 3)
let t2 = B 4 t1 t1
let t3 = B 5 (B 6 t1 t2) t2

let lt = [t1,t2,t3]

cat lt

map val lt
map children lt

sb t3

t3

let sb t = bfs [t]

:{
let bfs [] = []
    bfs t = map val t ++ bfs (concat (map children t))
:}

:{
let val (L a) = a
    val (B a _ _) = a
:}


:{
let children (L a) = []
    children (B a l r) = [l,r]
:}


-- doesn't work
:{
let val2 t
      | t == (L a) = a
      | t == (B a c d) = a
:}
