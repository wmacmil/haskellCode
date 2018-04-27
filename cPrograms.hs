
:{
let add1 [] = []
    add1 (x:xs) = (x+1) : add1 xs
:}

add1 [1..4]
add1 [1..4]


:{
let power x 0 = 1
    power x n = x * power x (n-1)
:}

power 4 3

if


4*262144

let addtwo (x:y:xs) = x + y

addtwo [1..3]

8 5 3 2 1 1



:{
let fasterFib 1 _ = [1,1]
    fasterFib n y@(x:x1:xs) = fasterFib (n-1) ff
      where ff = (x + x1) : y
:}

ff 1 = [1]
ff 2 = [1,1]
ff 3 = [2,1,1]

head [2,1,1]
head $ tail [2,1,1]

:{
let fib 1 = [1]
    fib 2 = [1,1]
    fib n = n' + n'' : fibNm1
      where n' = head fibNm1
            n'' = head $ tail fibNm1
            fibNm1 = fib (n-1)
:}

head $ fib 300000


let pl [x,y] = [(x+y),x]

pl [1,1]


:{
let fpl 2 = [1,1]
    fpl n = pl $ fpl (n-1)
:}

head $ fpl 300000

