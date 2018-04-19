

data Complex a
  = !a :+ !a    -- ^ forms a complex number from its real and imaginary
                -- rectangular components.
        deriving (Eq, Show, Read, Data, Generic, Generic1
                , Functor, Foldable, Traversable)

data Complex a = !a :+ !a deriving (Eq,Read,Show)

let a = Complex 3 4

let a = 3 :+ 4
let b = 0.3 :+ 4
let c = (:+) 3 4

let d = [a,b,c]

addC a b

foldr addC d

let functionOfList f [x,y] = f x y

let f x y = x + y

functionOfList f [3,4]

functionOfList (:+) [1,2]

:t a
:t b

realPart a
realPart b

:{
let realZ :: (RealFloat a) => Complex a -> a
    realZ (x :+ _) =  x
:}

:{
let imZ :: (RealFloat a) => Complex a -> a
    imZ (_ :+ y) =  y
:}


let addC z1 z2 = (realZ z1 + realZ z2) :+ (imZ z1 + imZ z2)

addC a b
-- alternatively

:{
let addComplex :: (RealFloat a) => Complex a -> Complex a -> Complex a
    addComplex (x :+ y) (x1 :+ y1) = (x + x1) :+ (y + y1)
:}

addComplex a b


let multz1z2 z1 z2 = (realZ z1 * realZ z2 - imZ z1 * imZ z2) :+ (realZ z1 * imZ z2 + realZ z2 * imZ z1)

multz1z2 a b
multz1z2 a c


data Complex a = !a :+ !a deriving (Eq, Show, Read, Functor, Foldable, Traversable)

:t functor

data Genre = Nonfiction | Novel | Biography deriving (Eq, Show)

