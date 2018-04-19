-- file: ch11/QC-basics.hs

:{
let qsort :: Ord a => [a] -> [a]
    qsort []     = []
    qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
        where lhs = filter  (< x) xs
              rhs = filter (>= x) xs
:}


let iprop_idempotent xs = qsort (qsort xs) == qsort xs

iprop_idempotent [1..5]

generate 10 (System.Random.mkStdGen 2) arbitrary :: [Bool]

import Data.List

import Test.Quickcheck



