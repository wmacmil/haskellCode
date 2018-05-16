
:{
data WeightCharacterTuple = WeightCharacterTuple {
        weight :: Int,
        character :: Char
}deriving (Show)
:}


:{
instance Eq WeightCharacterTuple where
        a == b = (weight a) == (weight b)
:}

:{
instance Ord WeightCharacterTuple where
        a >  b  = (weight a) >  (weight b)
        a >= b  = (weight a) >= (weight b)
        a <  b  = (weight a) <  (weight b)
        a <= b  = (weight a) <= (weight b)
:}

data Tree a = Node a (Tree a) (Tree a) | Leaf a
        deriving (Show)


instance Eq a => Eq (Tree a) where
        (==) a b        = (node a) == (node b)



node x = case x of
        (Node n _ _)    -> n
        (Leaf n)        -> n

instance Ord a => Ord (Tree a) where
        left >= right =  (node left) >= (node right)
        left <  right =  (node left) < (node right)
        left >  right =  (node left) > (node right)
        left <= right =  (node left) <= (node right)


toFrequencyCharacterTuple "asjdkfj jbjasjdfj jsdjfk jj sjjaj jejj jasjdfk j"

let asdf = toFrequencyCharacterTuple "abbcccddde eeeeeffff"

toFrequencyCharacterTuple "aababcccdddeeeffff"

-- this code is degenerate
toFrequencyCharacterTuple "aaabbbcccdddeeefff"



asdf

let a

:{
let toFrequencyCharacterTuple :: String -> [WeightCharacterTuple]
    toFrequencyCharacterTuple string = quickSort $ zipWith WeightCharacterTuple counts uniqueLetters
            where
            counts = map (frequency string) uniqueLetters
            frequency :: String -> Char -> Int
            frequency (x:xs) c
                    | c == x        = 1 + frequency xs c
                    | otherwise     = frequency xs c
            frequency _ c   = 0
            uniqueLetters = unique string
:}


:{
let unique :: String -> String
    unique (x:xs)   = [x] ++ unique [y | y <- xs, y /= x ]
    unique []               = []
:}



huffman :: [Tree WeightCharacterTuple] -> [Tree WeightCharacterTuple]
huffman (min1:min2:rest) = huffman newList
        where
        newList
                | length rest /= 0 =   quickSort ((merge min1 min2):rest)
                | otherwise = [merge min1 min2]
                        where merge a b
                                | a <= b = Node (WeightCharacterTuple newWeight '*') a b
                                | otherwise = Node (WeightCharacterTuple newWeight '*') b a
                                where newWeight = (weight (node a)) + (weight (node b))
huffman x = x

x `endsWith` y = (take (length y) (reverse x)) == (reverse y)

encode::WeightCharacterTuple -> Tree WeightCharacterTuple -> String
encode w (Node n left right) = oneOf ('0':(encode w left)) ('1':(encode w right))
        where oneOf x y
                | y `endsWith` "WRONG-LEAF"     = x
                | otherwise                     = y
encode w (Leaf l)
        | (character l) == (character w) = ""
        | otherwise = "WRONG-LEAF"


quickSort (x:xs) = l1 ++ [x] ++ l2 -- items less than x + x + items bigger than x
        where
                l1 = quickSort [y | y <- xs, y < x] -- sorted items less than x
                l2 = quickSort [y | y <- xs, y >= x] -- sorted items greater than x
quickSort [] = []


displayAllEncodings :: [WeightCharacterTuple] -> Tree WeightCharacterTuple -> String
displayAllEncodings (x:xs) tree= (codeForX x) ++ "\n" ++ (displayAllEncodings xs tree)
        where codeForX (WeightCharacterTuple w c) = (show c) ++ " weight = " ++ (show w) ++ " code = " ++ (encode x tree)
displayAllEncodings _ _ = []


main=do
        x <- getLine

        let tupleList=toFrequencyCharacterTuple x
        let inputTreeList=map Leaf tupleList
        let tree=huffman inputTreeList
        putStrLn (show tree)

        putStrLn (displayAllEncodings tupleList (tree!!0))

        return ()

