I'm trying to define fmap over a Tree type from hammar's answer in https://stackoverflow.com/questions/7624774/haskell-map-for-trees

His definition derives functor, which uses a pragma, which I'm only vaguely familiar with.  His definition is

    {-# LANGUAGE DeriveFunctor #-}
    data Tree a = Leaf a | Node (Tree a) (Tree a)
        deriving (Functor, Show)

I can't get the pragma and definition to work in GHCI.  Below are my three mistaken attempts, and I would appreciate any feedback!

First attempt:

    Prelude> {-# LANGUAGE DeriveFunctor #-}
    Prelude> data Tree a = Leaf a | Node (Tree a) (Tree a)
    Prelude>     deriving (Functor, Show)
    <interactive>:30:5: parse error on input ‘deriving’

Second try:

    Prelude> {-# LANGUAGE DeriveFunctor #-}
    Prelude> data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Functor, Show)
    <interactive>:32:57:
        Can't make a derived instance of ‘Functor Tree’:
      You need DeriveFunctor to derive an instance for this class
    In the data declaration for ‘Tree’

Third Try:

    Prelude> :{
    Prelude| {-# LANGUAGE DeriveFunctor #-}
    Prelude| data Tree a = Leaf a | Node (Tree a) (Tree a)
    Prelude|     deriving (Functor, Show)
    Prelude| :}
    <interactive>:35:1: parse error on input ‘data’



