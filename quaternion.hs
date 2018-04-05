

data Complex a
  = !a :+ !a    -- ^ forms a complex number from its real and imaginary
                -- rectangular components.
        deriving (Eq, Show, Read, Data, Generic, Generic1
                , Functor, Foldable, Traversable)

data Complex a = !a :+ !a deriving (Eq,Read,Show)

let a = Complex 3 4

let a = 3 :+ 4

let b = 0.3 :+ 4

:t a
:t b

realPart a
realPart b

:{
let realPart :: (RealFloat a) => Complex a -> a
    realPart (x :+ _) =  x
:}

:{
let addComplex :: (RealFloat a) => Complex a -> Complex a -> Complex a
    addComplex (x :+ y) (x1 :+ y1) = (x + x1) :+ (y + y1)
:}

addComplex a b


data Complex a = !a :+ !a deriving (Eq, Show, Read, Functor, Foldable, Traversable)

:t functor

data Genre = Nonfiction | Novel | Biography deriving (Eq, Show)

