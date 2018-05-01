GHCi> (5 + 6) + 10
21
GHCi> 5 + (6 + 10)
21


(5 + 6) + 10
5 + (6 + 10)


(5 - 6) - 10
5 - (6 - 10)

:{
class Monoid a where
    mempty  :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty
:}

:{
instance Monoid [a] where
    mempty  = []
    mappend = (++)
:}



mconcat :: [a] -> a
mconcat = foldr mappend mempty


-- | Monoid under addition.

newtype Sum a = Sum { getSum :: a }

-- | Monoid under multiplication.

newtype Product a = Product { getProduct :: a }

:{
instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum (x + y)
:}

(<>) Sum 5 Sum 6

Sum 5 <> Sum 6 <> Sum 10

instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)

