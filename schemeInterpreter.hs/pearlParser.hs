
newtype Parser a = Parser (String -> [(a,String)])


:{
let item :: Parser Char
    item  = Parser (\cs -> case cs of
                    ""     -> []
                    (c:cs) -> [(c,cs)])
:}


:{
class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
:}

:t return

:t Monad

:{
instance Monad Parser where
   return a = Parser (\cs -> [(a,cs)])
   p >>= f  = Parser (\cs -> concat [parse (f a) cs’ | (a,cs’) <- parse p cs])
:}


