-- ghci> :set -XRankNTypes
-- this needs to be set to use the forall

-- REMEMBER TO DO THIS

:set -XRankNTypes
:set -XExistentialQuantification

:{
let map :: forall a b. (a -> b) -> [a] -> [b]
    map f [] = []
    map f (x:xs) = f x : map f xs
:}

a :: Int

data ShowBox = forall s. Show s => SB s
:t SB

:{
let heteroList :: [ShowBox]
    heteroList = [SB (), SB 5, SB True]
:}



instance Show ShowBox where show (SB s) = show s

  -- (*) see the comment in the text below

:{
let f :: [ShowBox] -> IO ()
    f xs = mapM_ print xs
:}

:t print

:t True

let main = f heteroList

main

data T = forall a. MkT a


data T' = forall a. Show a => MkT' a

let heteroList' = [MkT' 5, MkT' (), MkT' True, MkT' "Sartre"]

let main = mapM_ (\(MkT' x) -> print x) heteroList'

main


{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

newtype Pair a b = Pair (forall c. (a -> b -> c) -> c)

newtype Pair a b = Pair {runPair :: forall c. (a -> b -> c) -> c}

:{
let makePair :: a -> b -> Pair a b
    makePair a b = Pair $ \f -> f a b
:}



let asdf = makePair 3 4
let wasdf = makePair 3 'a'

:t asdf
:t wasdf

runPair asdf (\x y -> x)




:t MkT

:t Cons



:t Cons

Cons 3 l2

:{
foob :: forall a b. (b -> b) -> b -> (a -> b) -> Maybe a -> b
foob postProcess onNothin onJust mval =
    postProcess val
    where
        val :: b
        val = maybe onNothin onJust mval
:}


data L x = Empty | Cons x (L x)

-- so Cons :: x -> L x -> L x

:t Empty

:{
let l1 :: L Int
    l1 = Cons 3 Empty
:}


let l2 = Cons 4 l1


:t l2

3

:t (==)


data B = T | F



eqB :: a -> b -> B
eqB



