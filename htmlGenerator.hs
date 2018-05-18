
-- Try to make an emmet vim in haskell

-- which syntax is better?

surround xs byY andZ = byY ++ xs ++ andZ

surroundbyYandZ xs y z = y ++ xs ++ z

surroundbyYandZ "asdj" "div" "vid"

tags xs = '<' : xs ++ ['>']

tags2 xs b a  = b ++ xs ++ a


tags "div"
endTags xs = "</" ++ xs ++ ['>']
endTags "div"

t x = tags2 x "<" ">\n"
et x = tags2 x "</" ">"

putStrLn $ tags2 "asjfjksd" "<" ">\n"

putStrLn $ elementize "div" "randomtext\n"

elementize "div" "randomtext"

putStrLn $ elementize "ul" $ elementize "div" "randomtext"

putStrLn $ elementize "div" "randomtext"

putStrLn $ addTab (elementize "div" "randomtext") '\n'

putStrLn $ elementize "li" $ elementize "div" "randomtext"

putStrLn $ elementize "li" $ addTab (elementize "div" "randomtext") '\n'

putStrLn $

-- there's a fundamental problem with how these are defined
putStrLn $ addTab (elementize "li" $ addTab (elementize "div" "randomtext") '\n') '\n'

putStrLn $ addTab (elementize "li" $ (elementize "div" "  randomtext")) '\n'

-- its like some kind of twisted dependency.  can't tab the second line without tabbing the last line
-- try this
realTab x = addTab (tab x) '\n'

putStrLn $ elementize "li" $ realTab $ elementize "div" $ realTab "asdf"

let a = nestedElements ["img","div","div"] "text"

let b = map (nestedElements "div") ["text","free"]


:{
let nestedElements [] y = putStrLn y
    nestedElements (x:xs) y = nestedElements xs er
      where er = elementize x $ realTab y
:}

:{
elementize tagname content = surround tabbedct tag endtag
  where tag = t tagname
        endtag = et tagname
        tabbedct = content ++ "\n"
:}

"\n" == "ab"

"\n"

addTab "anb\najsdfj"

addTab "anbbsdjfjbbajsdfj" "bb"

doubleCat "anbbsdjfjbbajsdfj"

doubleCat "anb\nbsdjfjbbajsdfj"

-- ah the \n is a char, not a string!

addTab "anb\nbsdjfjbbajsdfj" '\n'


map (\x -> x:[x]) [1..5]

mm f (x:xs) = f x :

doubleCat [1..5]
:{
doubleCat [] = []
doubleCat (x:xs) = x:x: doubleCat xs
:}

flatten

double x = x:x


tab x = "  " ++ x

addTab

:{
let addTab [] _ = []
    addTab (x:xs) z
      | x == z = z : "  " ++ addTab xs z
      | otherwise = x : addTab xs z
:}

:{
let addTab [x] _ = [x]
    addTab (x:y:xs) z
      | [x,y] == z = z ++ "  " ++ addTab xs z
      | otherwise = x : addTab (y:xs) z
:}

:{
let addTab [x] = [x]
    addTab (x:y:xs)
      | [x,y] == "\n" = "\n  " ++ addTab xs
      | otherwise = x : addTab (y:xs)
:}

'b'

'\\'

['\','n']



:{
elementize tagname content = surround content tag endtag
  where tag = tags tagname
        endtag = endTags tagname
:}

elementize "ul" $ elementize "div" "randomtext"

elementize "ul" $ elementize "div" "randomtext"

:t putStrLn

putStrLn $ "foo" ++ "\n" ++ "bar"

map (elementize "li") ["List1","List2","List3"]

