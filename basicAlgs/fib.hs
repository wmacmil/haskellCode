1
1,1
2,1,1
3,2,1,1
5,3,2,1,1


-- optimizes, could just use : a instead of : [head a]

:{
fib 0 = [1]
fib 1 = [1,1]
fib n = (head a) + (head $ tail a) : [head a]
  where a = fib (n-1)
:}

fib 99

head $ fib 99






