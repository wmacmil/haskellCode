-- how to get a powerset of a list


[1,2,3] = [1] !^ [2,3],[2],[3],[]

ps [] = []

ps [x] = [x],[]

1 : [2,3]

map (++ 1:) [2,3]

map (++ [1]) [[2,3],[2],[3],[]]

map ((:) 1) asdf ++ asdf

let asdf = [[2,3],[2],[3],[]]


:{
let ps [] = [[]]
    ps [x] = [[x],[]]
    ps (x:xs) = map ((:) x) pr ++ pr
      where pr = ps xs
:}

-- too long
ps ['a'..'z']

ps [1..20]

3 ^ 3
3 ^/ 3.0

ps [[]]


