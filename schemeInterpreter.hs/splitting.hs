import System.Process

readProcess "seq" ["1", "10"] ""

:{
let isPlus x
      | x == '+' = True
      | otherwise = False
:}

:{
let isSpacs x
      | x == ' ' = True
      | otherwise = False
:}

isSpacs ' '

map isSpacs "asdk j dkfjf j"



-- need to count which position you're at
--

head [3]

:t tokenizeSpaces

:{
let tokenizeSpaces [] [y] a = y:a
    tokenizeSpaces (s:ss) (x:xs) a
      | s /= ' ' = tokenizeSpaces ss ((s:x):xs) a
      | otherwise = tokenizeSpaces ss xs (x:a)
:}

let s2 = "asdkjf jdfkfjdkdjd jdjj jd"

-- ahhh cs is improperly defined
cs s2

tokenizeSpaces s2 (nXs 0 []) []

tokenizeSpaces "asdf" (nXs 0 []) []

nXs 0 []

:{
let tk s = tokenizeSpaces s (nXs asdf []) []
      where asdf = cs s
:}

nXs (cs s2) []

tk s2

cs s2
:t cs


:{
let ts string = tokenizeSpaces string emptys []
      where emptys = nXs spaces []
            spaces = countSpaces string
            countSpaces = length . filter (isSpacs ' ')
:}

:{
let ts str = tokenizeSpaces str (nXs (cs str) []) []
:}

ts "askjdf jfjdjk askdj"

-- here's the final fcn
reverse $ map reverse $ ts "askjdf jfjdjk askdj"


:{
let listTil c [] = []
    listTil c (x:xs)
      | x /= c = x : listTil c xs
      | otherwise = []
:}

listTil ' ' "asdjf asdf"
listTil ' ' "asdf"

:{
let listTil c [] = []
    listTil c (x:xs)
      | x /= c = x : listTil c xs
      | otherwise = []
:}

:{
let listAfterC c [] = []
    listAfterC c (x:xs)
      | x == c = xs
      | otherwise = listAfterC c xs
:}

listTil ')' $ listAfterC '(' "asdjfk (+ 3 2)"

listAfterC ' ' "asdjf asdf"
listAfterC ' ' "asdf"
listAfterC ' ' ""

:{
let reltc c [] = []
    reltc c xs = listTil c xs : reltc c (listFromC c xs)
:}

reltc ' ' "asjkd jfdjkfjd jd jddks"



splitL :: Int -> [x] -> [[x],[x]]
splitL 0 xs b = head b : (xs : tail b)
splitL n (x:xs) b = splitL (n-1) xs (consHead x b)

-- note the first half of the list gets reversed

:{
let splitL 0 xs b = head b : (xs : tail b)
    splitL n (x:xs) b = splitL (n-1) xs (consHead x b)
:}

splitL 4 [1..10] [[]]

let consHead x b = (x : head b) : (tail b)


consHead 3 [[],[]]

[3] : [[]]


































let s1 =  "askjdf jfjdjk askdj"
cs s1
nXs (cs s1) []



cs

cs "asdf ddsa d sfsd 3k"

let cs = (+1) . length . filter (isSpacs ' ')

length $ filter (isSpacs ' ') "asd dfj d"

:{
let isSpacs c x
      | x == c = True
      | otherwise = False
:}


:{
let nXs 0 x = []
    nXs n x = x : nXs (n-1) x
:}
nXs 3 4


tokenizeSpaces "askdj jfjkd j dkk" [[],[],[],[]] []


parseStringforPlus

map isPlus "+*/-"

[1,2,3] == "123"
[1,2,3] == [1,2,3]

-- how to seperate all parens?

reltc ' ' "asjkd jfdjkfjd jd jddks"

listTil ')' $ listAfterC '(' "(+ 3 2)"

-- first replace
:{
let replace c str [] = []
    replace c str (x:xs)
      | c == x = str ++ replace c str xs
      | otherwise = x : replace c str xs
:}

3 == (1+2) && (1+2) == (1+1+1)

:{
let dd c [] = []
    dd c [x] = [x]
    dd c (x:y:xs)
      | x == y && y == c = dd c (y:xs)
      | otherwise = x : dd c (y:xs)
:}

dd ' ' "asj jdj jd j  djj jdjf j   d j jjd"

replace '(' " ( "  "(+ 3 2)"

-- assuming it begins with a ' '


-- tokenize
let x = tail $ reltc ' ' $ dd ' ' $ repParens "(+ 3 (+ 43 2393))"

:t x == [[Char]]

["string",["string","string"]]

-- need to construct a data type to make the ast

data Ast x = Val x | Node [Ast x] deriving (Show)

data Ast x = Val x | Node [Ast x] deriving (Show)

ast :: Ast [Char]

let ast = Node [(Val "asdf"),(Val "asdf")]
let ast2 = Node [ast,ast,ast,(Val "jdf")]

:{
let ast3 :: Ast [Char]
    ast3 = Node []
:}


let ast1 = ["asd"["asd","djfk"]]

let ast1 = ["asd"["asd","djfk"]]

data A a = a | [A]

-- no idea what's going wrong

:{
let parse [] = []
    parse (x:xs)
      | x == "(" = Node [parse xs]
      | x == ")" = []
      | otherwise = (Val x) : parse xs
:}


map (\x -> replace x "(+ 3 2)") ['(',')']

replace ')' $ replace '(' "(+ 3 2)"

let repParens x = replace '(' $ replace ')' x



reltc ' ' $ repParens "(+ 3 (+ 4 2))"


reltc ' ' "(+ 3 (+ 4 2))"


 "(+ 3 (+ 4 2))"

reltc2 '(' "(+ 3 (+ 4 2))"

reltc2 '(' $ reltc ' ' "(+ 3 (+ 4 2))"

:t reltc

:{
let reltc2 c [] = []
    reltc2 c xs = listTil c xs : [c] : reltc2 c (listFromC c xs)
:}

:{
let replace c [] = []
    replace c (x:xs)
      | c == x = str ++ replace c xs
      | otherwise = x : replace c xs
      where str = space:c:space:[]
            space = ' '
:}



reltc ' ' $ listTil ')' $ listAfterC '(' "(+ 3 2)"

reltc ' ' " j sdfj"

listTil ' ' " ajsd"


:{
let listTil c [] = []
    listTil c (x:xs)
      | x /= c = x : listTil c xs
      | otherwise = []
:}

:{
let listTil c [] = []
    listTil c (x:xs)
      | x /= c = x : listTil c xs
      | otherwise = []
:}


listAfterC

listAfterC '('

-- so we should either be looking for spaces or parens



-- how to do it with multiple lists in lists?

-- >> program = "(begin (define r 10) (* pi (* r r)))"
-- >>> parse(program)
-- ['begin', ['define', 'r', 10], ['*', 'pi', ['*', 'r', 'r']]]

