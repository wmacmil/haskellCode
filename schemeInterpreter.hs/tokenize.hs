-- cool, i think this should work
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

program = "(begin (define r 10) (* pi (* r r)))"

tokenize program

