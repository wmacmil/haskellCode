
module Tree where

data BinaryTree a = Leaf a | Branch a (BinaryTree a) (BinaryTree a) deriving (Show)


:{
let t1 :: BinaryTree Int
    t1 = Leaf 3
:}

t1

let t2 = Branch 4 t1 t1

let t3 = Branch 3 (Branch 4 (Node 2) (Node 3)) (Branch 9 (Node 2) (Node 3))

let t4 = Branch 93 t3 t2

t4

:{
let mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
    mapTree f (Node a) = Node (f a)
    mapTree f (Branch a l r) = Branch (f a) (mapTree f l) (mapTree f r)
:}



:{
let preOrderTreeTraversal (Node a) = [a]
    preOrderTreeTraversal (Branch a l r) = a : (preOrderTreeTraversal l) ++ (preOrderTreeTraversal r)
:}

preOrderTreeTraversal t4



inOTT (Node a) = a
inOTT (Branch a (Node b) r) = inOTT r
inOTT (Branch a l r) = a : (inOTT l)





(Branch a l r) = bfs l


:{
let val (Node a) = a
    val (Branch a l r) = a
:}


val (Node 3)

:{
let childrenVal (Node a) = []
    childrenVal (Branch a l r) = val l : val r
:}

[] : [3]

:{
let traverseBF :: BinaryTree a -> [a]
    traverseBF tree = tbf [tree]
        where
            tbf [] = []
            tbf xs = map nodeValue xs ++ tbf (concat (map leftAndRightNodes xs))
            nodeValue (Leaf a)       = a
            nodeValue (Branch a _ _) = a
            leftAndRightNodes (Branch _ (Leaf a) (Leaf b) = []
            leftAndRightNodes (Branch _ (Leaf a) b)       = [b]
            leftAndRightNodes (Branch _ a (Leaf b))       = [a]
            leftAndRightNodes (Branch _ a b)              = [a,b]
:}






