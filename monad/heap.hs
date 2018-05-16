

-- heapify, from ll to heap


data Heap a = Leaf a | Branch a (Heap a) (Heap a) deriving (Show,Eq)


heapify :: [x] -> Heap x
heapify (x:y:z:xs) = Branch x (


