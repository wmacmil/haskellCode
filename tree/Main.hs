module Main where

-- The main definition of a tree.
data Tree a = Leaf a | Branch a (Tree a) (Tree a) deriving (Show)

-- Depth-first tree traversal.
depthFirst                       :: Tree a -> [a]
depthFirst (Leaf x)              =  [x]
depthFirst (Branch x left right) =  depthFirst left ++ [x] ++ depthFirst right

-- Breadth-first tree traversal.
breadthFirst   :: Tree a -> [a]
breadthFirst x =  _breadthFirst [x]

_breadthFirst    :: [Tree a] -> [a]
_breadthFirst [] =  []
_breadthFirst xs =  map treeValue xs ++
                    _breadthFirst (concat (map immediateChildren xs))

-- Get the value of a given tree.
treeValue                         :: Tree a -> a
treeValue (Leaf x)                =  x
treeValue (Branch x left right)   =  x

-- Get the immediate children of a tree.
immediateChildren                       :: Tree a -> [Tree a]
immediateChildren (Leaf x)              =  []
immediateChildren (Branch x left right) =  [left, right]


-- Define some tree.
mytree = Branch "1"
  (Branch "2"
    (Leaf "4")
    (Leaf "5"))
  (Branch "3"
    (Leaf "6")
    (Leaf "7"))


{-     1
      /      2   3
    / \ /    4  5 6  7

   Here's another.
   mytree = Branch "0"
     (Leaf "1")
     (Branch "2"
       (Branch "3"
         (Leaf "4")
         (Leaf "5"))
       (Leaf "6"))

     0
    /    1   2
      /      3   6
    /    4   5 -}

-- Create one "do" out of a list of things to do.
doList :: [IO ()] -> IO ()
doList =  foldr (>>) (return ())




-- Main.
main :: IO ()
main = do doList (map putStrLn (breadthFirst mytree))
