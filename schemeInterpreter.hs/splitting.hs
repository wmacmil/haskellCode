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

tokenizeSpaces s2 (nXs 4 []) []

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

-- I think everthing before this point is a nice alternative, but I define it more efficiently below


:{
let listTil c [] = []
    listTil c (x:xs)
      | x /= c = x : listTil c xs
      | otherwise = []
:}

listTil ' ' "asdjf asdf"
listTil ' ' "asdf"


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


-- reltc === separating the input string by the char c
:{
let reltc c [] = []
    reltc c xs = listTil c xs : reltc c (listAfterC c xs)
:}

reltc ' ' "asjkd jfdjkfjd jd jddks"

-- splitL :: Int -> [x] -> [[x],[x]]
-- wrong type signature
splitL 0 xs b = head b : (xs : tail b)
splitL n (x:xs) b = splitL (n-1) xs (consHead x b)


--
-- note the first half of the list gets reversed
-- in splitL


consHead x b = (x : head b) : (tail b)
:{
let splitL 0 xs b = head b : (xs : tail b)
    splitL n (x:xs) b = splitL (n-1) xs (consHead x b)
:}

splitL 4 [1..10] [[]]

consHead 3 [[],[]]

-- note there was a big gap here earlier

let s1 =  "askjdf jfjdjk askdj"

cs s1

nXs (cs s1) []

:t cs

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

-- delete double of a char
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
-- lines 80 thru 110 to get reltc
-- repParens replaces every instance of ( and ) with ' ( ' and ' ) ' using the replace fcn, which I have two of defined above
-- dd deletes double of the spaces, therefore getting rid of instances of double spaces, i.e. '  ' occuring with two adjacent parens
-- reltc converts the str into a [] with ' ' as the separting character
-- since the initial '(' will always be converted to as string starting with ' ', tail gets rid of this issue

:{
let listTil c [] = []
    listTil c (x:xs)
      | x /= c = x : listTil c xs
      | otherwise = []
:}

listTil ' ' "asdjf asdf"
listTil ' ' "asdf"


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

-- reltc === separating the input string by the char c
:{
let reltc c [] = []
    reltc c xs = listTil c xs : reltc c (listAfterC c xs)
:}

reltc ' ' "asjkd jfdjkfjd jd jddks"
-- delete double of a char
:{
let dd c [] = []
    dd c [x] = [x]
    dd c (x:y:xs)
      | x == y && y == c = dd c (y:xs)
      | otherwise = x : dd c (y:xs)
:}

dd ' ' "asj jdj jd j  djj jdjf j   d j jjd"

:{
let replace c [] = []
    replace c (x:xs)
      | c == x = str ++ replace c xs
      | otherwise = x : replace c xs
      where str = space:c:space:[]
            space = ' '
:}

replace '(' "(+ 3 2)"

let repParens x = replace '(' $ replace ')' x

let x = tail $ reltc ' ' $ dd ' ' $ repParens "(+ 3 (+ 43 2393))"

tail $ reltc ' ' $ dd ' ' $ repParens program

tokenize str = tail $ reltc ' ' $ dd ' ' $ repParens $ str

tokenize program

tokenize = tail $ reltc ' ' $ dd ' ' $ repParens

program = "(begin (define r 10) (* pi (* r r)))"

let astTokenized = tokenize program

x

:t x == [[Char]]

["string",["string","string"]]

-- need to construct a data type to make the ast

data Ast x = Val x | Node [Ast x] deriving (Show)


ast :: Ast [Char]

let ast = Node [(Val "asdf"),(Val "asdf")]

let a1 = Node []

:t a1

let ast2 = Node [ast,ast,ast,(Val "jdf")]

:t ast2

ast

ast2


-- no idea what's going wrong
-- parse :: [[Char]] -> Ast [Char]
--
-- this typechecks

I'm trying to write a lisp interpreter in haskell, inspired by Norvig's in Python (http://norvig.com/lispy.html).  I have a successful tokenizer which I can link to if need be.  Here it outputs the correct code up to Norvig's Python tokenizer.

    program = "(begin (define r 10) (* pi (* r r)))"
    astTokenized = tokenize program
    astTokenized == ["(","begin","(","define","r","10",")","(","*","pi","(","*","r","r",")",")",")"]

Here I define my abstract syntax tree data type, although I know that it already has some implicit error as it isn't wrapped in a list.

    data Ast x = Val x | Node [Ast x] deriving (Show)

Here was my first try:

    :{
    parse :: [[Char]] -> [Ast [Char]]
    parse (x:xs)
      | x == "(" = [Node (parse xs)]
      | x == ")" = []
      | otherwise = (Val x) : parse xs
    :}

Hopeful, except for it terminates after the first ')'.

    Prelude> parse astTokenized
    [Node [Val "begin",Node [Val "define",Val "r",Val "10"]]]

Here I change the condition for ')' so it will terminate, but it now just creates a deeper tree, therefore failing to branch correctly.

:{
parse [] = []
parse (x:xs)
  | x == "(" = [Node (parse xs)]
  | x == ")" = parse xs
  | otherwise = (Val x) : parse xs
:}

In some way it needs to allow for "parallel" trees, not merely nested.  Any help would be appreciated.

-- :{
-- parse (x:xs)
--   | x == "(" = [Node (parse xs)]
--   | x == ")" = parse xs
--   | otherwise = (Val x) : parse xs
-- :}

:t parse

parse astTokenized

astTokenized

let astTokenized = tokenize program

:{
let ast3 :: Ast [Char]
    ast3 = Node []
:}


let ast1 = ["asd"["asd","djfk"]]

let ast1 = ["asd"["asd","djfk"]]

data A a = a | [A]



map (\x -> replace x "(+ 3 2)") ['(',')']

replace ')' $ replace '(' "(+ 3 2)"

let repParens x = replace '(' $ replace ')' x

reltc ' ' $ repParens "(+ 3 (+ 4 2))"

repParens "(+ 3 (+ 4 2))"
reltc ' ' "(+ 3 (+ 4 2))"


 "(+ 3 (+ 4 2))"

reltc2 '(' "(+ 3 (+ 4 2))"

reltc2 '(' $ reltc ' ' "(+ 3 (+ 4 2))"

:t reltc

:{
let reltc2 c [] = []
    reltc2 c xs = listTil c xs : [c] : reltc2 c (listAfterC c xs)
:}

:{
let replace c [] = []
    replace c (x:xs)
      | c == x = str ++ replace c xs
      | otherwise = x : replace c xs
      where str = space:c:space:[]
            space = ' '
:}
replace '(' "(+ 3 2)"

reltc ' ' $ listTil ')' $ listAfterC '(' "(+ 3 2)"


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

-- tokenize
-- lines 80 thru 110 to get reltc
-- repParens replaces every instance of ( and ) with ' ( ' and ' ) ' using the replace fcn, which I have two of defined above
-- dd deletes double of the spaces, therefore getting rid of instances of double spaces, i.e. '  ' occuring with two adjacent parens
-- reltc converts the str into a [] with ' ' as the separting character
-- since the initial '(' will always be converted to as string starting with ' ', tail gets rid of this issue

:{
let listTil c [] = []
    listTil c (x:xs)
      | x /= c = x : listTil c xs
      | otherwise = []
:}

listTil ' ' "asdjf asdf"
listTil ' ' "asdf"


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

-- reltc === separating the input string by the char c
:{
let reltc c [] = []
    reltc c xs = listTil c xs : reltc c (listAfterC c xs)
:}

reltc ' ' "asjkd jfdjkfjd jd jddks"
-- delete double of a char
:{
let dd c [] = []
    dd c [x] = [x]
    dd c (x:y:xs)
      | x == y && y == c = dd c (y:xs)
      | otherwise = x : dd c (y:xs)
:}

dd ' ' "asj jdj jd j  djj jdjf j   d j jjd"

:{
let replace c [] = []
    replace c (x:xs)
      | c == x = str ++ replace c xs
      | otherwise = x : replace c xs
      where str = space:c:space:[]
            space = ' '
:}

replace '(' "(+ 3 2)"

let repParens x = replace '(' $ replace ')' x

let x = tail $ reltc ' ' $ dd ' ' $ repParens "(+ 3 (+ 43 2393))"

tail $ reltc ' ' $ dd ' ' $ repParens program

tokenize str = tail $ reltc ' ' $ dd ' ' $ repParens $ str

tokenize program




I'm trying to write a lisp interpreter in haskell, inspired by Norvig's in Python (http://norvig.com/lispy.html).  I have a successful tokenizer which I can link to if need be.  Here it outputs the correct code up to Norvig's Python tokenizer.

    program = "(begin (define r 10) (* pi (* r r)))"
    astTokenized = tokenize program
    astTokenized == ["(","begin","(","define","r","10",")","(","*","pi","(","*","r","r",")",")",")"]

Here I define my abstract syntax tree data type, although I know that it already has some implicit error as it isn't wrapped in a list.

    data Ast x = Val x | Node [Ast x] deriving (Show)

Here was my first try:

    parse :: [[Char]] -> [Ast [Char]]
    parse (x:xs)
      | x == "(" = [Node (parse xs)]
      | x == ")" = []
      | otherwise = (Val x) : parse xs

Hopeful, except for it terminates after the first ')'.

    Prelude> parse astTokenized
    [Node [Val "begin",Node [Val "define",Val "r",Val "10"]]]

Here I change the condition for ')' so it will terminate, but it now just creates a deeper tree, therefore failing to branch correctly.

    parse [] = []
    parse (x:xs)
      | x == "(" = [Node (parse xs)]
      | x == ")" = parse xs
      | otherwise = (Val x) : parse xs

    Prelude> parse astTokenized
    [Node [Val "begin",Node [Val "define",Val "r",Val "10",Node [Val "*",Val "pi",Node [Val "*",Val "r",Val "r"]]]]]

In some way it needs to allow for "parallel" trees, not merely nested.  Any help would be appreciated.
