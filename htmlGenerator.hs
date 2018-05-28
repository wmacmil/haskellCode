
-- Try to make an emmet vim in haskell

-- which syntax is better?

surround xs byY andZ = byY ++ xs ++ andZ

surroundbyYandZ xs y z = y ++ xs ++ z

surroundbyYandZ "asdj" "div" "vid"

tags xs = '<' : xs ++ ['>']



tags "div"

tags2 xs b a  = b ++ xs ++ a

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

a

let a = nestedElements ["img","div","div"] "text"

let b = map (nestedElements ["div"]) ["text","free"]

-- cool, this is how you parallelize it
mapM (nestedElements ["div"]) ["text","free"]

:t mapM (nestedElements ["div"]) ["text","free"]

nestedElements ["div"] "text"

nestedElements ["div"] "text"

nestedElements ["html"] (nestedElements ["div"] "text")

:t mapM

:t nestedElements ["div"]
:t nestedElements

mapIO (nestedElements ["div"]) ["text","free"]

mapIO nestedElements ["div"] ["text","free"]

-- not right, needs some correction
-- need to get rid of the extra space at the end

putStrLn $ foldl (\x y -> x ++ "\n" ++ y) "" $ map (nestedElements2 ["div"]) ["text","free","point"]

map (nestedElements2 ["div"]) ["text","free"]

:t nestedElements

:{
mapIO :: (a -> IO b) -> [a] -> [b] -> IO [b]
mapIO f [] acc = do return acc
mapIO f (x:xs) acc = do
  new <- f x
  mapIO f xs (new:acc)
:}

-- correct

-- now its composable
nestedElements2 ["bjsdjf"] $  nestedElements2 ["akd"] "text"

:{
let nestedElements2 [] y = y
    nestedElements2 (x:xs) y = nestedElements2 xs er
      where er = elementize x $ realTab y
:}

-- correct
:{
let nestedElements [] y = putStrLn y
    nestedElements (x:xs) y = nestedElements xs er
      where er = elementize x $ realTab y
:}

:t realTab

:{
elementize tagname content = surround tabbedct tag endtag
  where tag = t tagname
        endtag = et tagname
        tabbedct = content ++ "\n"
:}

-- i think this is the one used in the above defn
-- the second char should be \n to get realtab above
:{
let addTab [] _ = []
    addTab (x:xs) z
      | x == z = z : "  " ++ addTab xs z
      | otherwise = x : addTab xs z
:}

:t addTab

tab x = "  " ++ x


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



addTab


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

