
:{
let fibs 0 = 0
    fibs 1 = 1
    fibs n = fibs (n-1) + fibs (n-2)
:}

-- do not exceed 4 million
-- even valued terms


-- calculate all 3 digit binary multiplications

if you do it in reverse it should just be the largest palindrome




numToList x =

:{
let reverseNum :: Int -> Int
    reverseNum = read . reverse . show
:}



reverseNum 34

reverseInt x = (*) (signum x) . read . reverse . show . abs  $ x


-- isPalindrome was originally mapping to {0,1}
-- respell to palindrome

:{
let isPalnindrome1 :: Int -> Bool
    isPalnindrome1 x = if (((reverseNum x) - x) == 0)
                      then True
                      else False
:}


reverse "abc"

:{
let myRev [] = []
    myRev (x:xs) = myRev xs ++ [x]
:}

myRev [1..4]

isPalnindrome1 34343

isPalnindrome1 34342

map isPalnindrome1 [1,2,3,43,4334]

filter isPalnindrome1 [2,3,4343,434]

map isPalindrome1 [1,2,3,43,4334]

map (+3)  [1,2,3,43,4334]

[999..1]

map (*3) [1..33]

let p x xs = map (*x) xs

p 3 [3,4]

pairsOf ::

:t pairsOf

:{
let pairsOf f [] ys = []
    pairsOf f (x:xs) ys = f x ys : pairsOf f xs ys
:}

let threeDigits = reverse [500..999]
threeDigits

-- takes two lists and returns a list of lists , a combination/combinationorial product in some sense
let big3DigitMultiples = pairsOf p threeDigits threeDigits

let flattenLists = foldl (++) []

flattenLists [[1..33],[5..56]]

let q = flattenLists big3DigitMultiples

let t = filter isPalnindrome1 q

-- correct solution, althought I need to go back and implement a maximum function
maxi t


import Data.List
import Data.Ord

let maxi xs = maximumBy (comparing fst) (zip xs [0..])


maxList (x:xs) =


-- wrong because ++ has the correct type signature
:{
let unList [] = []
    unList (x:xs) = x: unlist xs
:}

curiousNum x =
x 10

:{
let isPalnindrome1 :: Int -> Bool
    isPalnindrome1 x = if (((reverseNum x) - x) == 0)
                      then True
                      else False
:}


let square x = x^2

square 34


let foldMap f = foldl (+) 0 (map f x

sumSquare = foldl (+) 0 map square [1..100]

let sumSquare = foldl (+) 0 b

sumSquare

let b = map square [1..100]


let sumL x = foldl (+) 0 x

sumL [1..100]

let squareSum = square $ sumL [1..100]

squareSum - sumSquare
squareSum - sumSquare


zero
one
two
three
four
five
six
seven
eight
nine
ten
eleven
twelve
thirteen
fourteen
fifteen
sixteen
seventeen
eighteen
nineteen
twenty
thiry
forty
fifty
sixty
seventy
eighty
ninety
hundred

-- pentagonal numbers problem
let pentagonal n = n * (3 * n - 1) / 2

let x y = map pentagonal [1..y]

x 10


let p2 x xs = map (+x) xs

-- (-x) wouldn't work
let p3 x xs = map (subtract x) xs

-- takes two lists and returns a list of lists , a combination/combinationorial product in some sense
--
let big3DigitMultiples2 = pairsOf p2 (x 20) (x 20)

-- intersect two lists

let big3DigitMultiples3 = pairsOf p3 (x 20) (x 20)

big3DigitMultiples2



let asdf = foldl  (+) 0 . map (*3)

asdf [1..234]

let fd = map (+3)

let df = foldl (+) 0

let sumList = foldl (+) 0


let dffd = df . fd

dffd [1..20]

:t df


df [2,3,4]


let k = zipWith (*)

k [3

let vectorProduct = foldl (+) 0 . zipWith (*)

let vectorProduct1 = foldl $ (+) $ 0 $ zipWith $ (*)

let vectorProduct x y = df (k x y)

-- vector product using sumlist

let k = zipWith (*)
let vectorP x y = df $ k x y

-- i used k instead of vectorP
let matrixP x y = map (k $ x) y

let matrixPr x y = map (vectorP $ x) y

-- noncommutative with the foldl operation

let matrixPro x y = map (vectorP $ y) x

matrixPro [[1..4],[5..8],[3,5,3,1]] [3,5,2,0]

matrixPr [3,5,2,0] [[1..4],[5..8],[3,5,3,1]]

let matrixMatrixPr a b = foldl (matrixPr $ a) b

-- this works, i had a noncommutative input type apparently

let matrixMatrixPr a b = map (matrixPro $ b) a



-- This now allows one to fold with matrix product, the identity matrix being unique for a given dimension

let manyMatrixPr = foldl matrixMatrixPr

foldl (matrixMatrixPr) [[1,0],[0,1]] [[[3,4],[3,2]],[[1,0],[1,34]],[[1,0],[1,1]]]




manyMatrixPr [[1,0],[0,1]] [[[3,4],[3,2]],[[1,0],[1,34]],[[1,0],[1,1]]]

:t ma

matrixMatrixPr [[3,4],[3,2],[2,7]] [[1,0],[-2,3],[1,1]]

matrixMatrixPr [[3,4],[3,2]] [[1,0],[1,1]]


matrixP [3,5,2,0] [[1..4],[5..8]]

let bilinearP x y z = vectorP z $ matrixPr x y

-- could also define this with matrixPro appled to a vectorP due to commutativity

bilinearP [3,5,2,0] [[1..4],[5..8]] [3,21,3]


:{
let oneAtI 1 = 1 : []
    oneAtI n = 0 : oneAtI (n-1)
:}

oneAtI 3

:{
let nZeros 0 = []
    nZeros n = 0 : nZeros (n-1)
:}

nZeros 30
nZeros 3

-- this solves the problem earlier of needing to "keep track" of the invariant dimension

let po x y = (oneAtI x) ++ (nZeros (y-x))

po 3 8

let op y x = po x y

op 8 3

map (op 8) [1..8]

let identity n = map (op n) [1..n]

identity 10


-- swapInd [1..6] 2 3 evaluates to [1,3,2,4..6]
--


:{
let retrieveNthIndex 1 (x:xs) = x
    retrieveNthIndex n (x:xs) = retrieveNthIndex (n-1) xs
:}

-- This returns the same value as !!

:{
let retrieveNthIndex2 0 (x:xs) = x
    retrieveNthIndex2 n (x:xs) = retrieveNthIndex2 (n-1) xs
:}

retrieveNthIndex 3 [1..23]

retrieveNthIndex 6 [1,23,4,5,3,2,123]

retrieveNthIndex2 6 [1,23,4,5,3,2,123]

[1,23,4,5,3,2,123] !!6

-- check the other file for a properly defined splitAt Function (using take and leave).  once we have this defined it is easy to modify the list as needed

:{
let changeNthIndex _ 0 y = y
    changeNthIndex (x:xs) n y = x : changeNthIndex (n-1) y
:}

splitAt (x:xs) n = x : splitAt xs (n-1)

swapInd (x:xs) n m = x : swapInd (n-1) (m -1 )


-- not quite, need to define it so that it has [1,0,..,0]
--
let reverseOneAtI = reverse . oneAtI

reverseOneAtI 4

:{
let oneAtILengthN n 0 = oneAtI n
    oneAtILengthN n m = 0 : oneAtILengthN n (m-1)
:}


:{
let oneAtILengthN n 0 = reverseOneAtI n
    oneAtILengthN n m = 0 : oneAtILengthN n (m-1)
:}

-- need to maintain an invariant sum of the parameters, thinking of an easier way

oneAtILengthN 4 6

oneAtILengthN 8 2

oneAtILengthN 2 8

oneAtILengthN 4 6

oneAtILengthN 1 8

let b x y = oneAtILengthN y x


map (b $ 4) [1..4]


-- how to do this to a bunch of items on a list?
map

-- permutation matrix
-- input: well typed indices to be permuted
-- output: correct matrix
-- example: [1,3,2,4] -> [[1,0,0,0],[0,0,1,0],[0,1,0,0],[0,0,0,1]]

matrixPr [3,1,2,1] [[1,0,0,0],[0,0,1,0],[0,1,0,0],[0,0,0,1]]




let replicateM n = sequence . replicate n

replicateM 3 [3,2,4]
replicateM 3 [1,0,0]

let vp x y = fold $ (+) $ 0 (zipWith $ (*) $ x $ y)

let vp = fold $ (+) $ 0 . zipWith $ (*)

vectorProduct

vectorProduct [1..10] [2..20]

k [1..10] [2..20]










[37107287533902102798797998220837590246510135740250]
[46376937677490009712648124896970078050417018260538]
[74324986199524741059474233309513058123726617309629]
[91942213363574161572522430563301811072406154908250]
[23067588207539346171171980310421047513778063246676]
[89261670696623633820136378418383684178734361726757]
[28112879812849979408065481931592621691275889832738]
[44274228917432520321923589422876796487670272189318]
[47451445736001306439091167216856844588711603153276]
[70386486105843025439939619828917593665686757934951]
[62176457141856560629502157223196586755079324193331]
[64906352462741904929101432445813822663347944758178]
[92575867718337217661963751590579239728245598838407]
[58203565325359399008402633568948830189458628227828]
[80181199384826282014278194139940567587151170094390]
[35398664372827112653829987240784473053190104293586]
[86515506006295864861532075273371959191420517255829]
[71693888707715466499115593487603532921714970056938]
[54370070576826684624621495650076471787294438377604]
[53282654108756828443191190634694037855217779295145]
[36123272525000296071075082563815656710885258350721]
[45876576172410976447339110607218265236877223636045]
[17423706905851860660448207621209813287860733969412]
[81142660418086830619328460811191061556940512689692]
[51934325451728388641918047049293215058642563049483]
[62467221648435076201727918039944693004732956340691]
[15732444386908125794514089057706229429197107928209]
[55037687525678773091862540744969844508330393682126]
[18336384825330154686196124348767681297534375946515]
[80386287592878490201521685554828717201219257766954]
[78182833757993103614740356856449095527097864797581]
[16726320100436897842553539920931837441497806860984]
[48403098129077791799088218795327364475675590848030]
[87086987551392711854517078544161852424320693150332]
[59959406895756536782107074926966537676326235447210]
[69793950679652694742597709739166693763042633987085]
[41052684708299085211399427365734116182760315001271]
[65378607361501080857009149939512557028198746004375]
[35829035317434717326932123578154982629742552737307]
[94953759765105305946966067683156574377167401875275]
[88902802571733229619176668713819931811048770190271]
[25267680276078003013678680992525463401061632866526]
[36270218540497705585629946580636237993140746255962]
[24074486908231174977792365466257246923322810917141]
[91430288197103288597806669760892938638285025333403]
[34413065578016127815921815005561868836468420090470]
[23053081172816430487623791969842487255036638784583]
[11487696932154902810424020138335124462181441773470]
[63783299490636259666498587618221225225512486764533]
[67720186971698544312419572409913959008952310058822]
[95548255300263520781532296796249481641953868218774]
[76085327132285723110424803456124867697064507995236]
[37774242535411291684276865538926205024910326572967]
[23701913275725675285653248258265463092207058596522]
[29798860272258331913126375147341994889534765745501]
[18495701454879288984856827726077713721403798879715]
[38298203783031473527721580348144513491373226651381]
[34829543829199918180278916522431027392251122869539]
[40957953066405232632538044100059654939159879593635]
[29746152185502371307642255121183693803580388584903]
[41698116222072977186158236678424689157993532961922]
[62467957194401269043877107275048102390895523597457]
[23189706772547915061505504953922979530901129967519]
[86188088225875314529584099251203829009407770775672]
[11306739708304724483816533873502340845647058077308]
[82959174767140363198008187129011875491310547126581]
[97623331044818386269515456334926366572897563400500]
[42846280183517070527831839425882145521227251250327]
[55121603546981200581762165212827652751691296897789]
[32238195734329339946437501907836945765883352399886]
[75506164965184775180738168837861091527357929701337]
[62177842752192623401942399639168044983993173312731]
[32924185707147349566916674687634660915035914677504]
[99518671430235219628894890102423325116913619626622]
[73267460800591547471830798392868535206946944540724]
[76841822524674417161514036427982273348055556214818]
[97142617910342598647204516893989422179826088076852]
[87783646182799346313767754307809363333018982642090]
[10848802521674670883215120185883543223812876952786]
[71329612474782464538636993009049310363619763878039]
[62184073572399794223406235393808339651327408011116]
[66627891981488087797941876876144230030984490851411]
[60661826293682836764744779239180335110989069790714]
[85786944089552990653640447425576083659976645795096]
[66024396409905389607120198219976047599490197230297]
[64913982680032973156037120041377903785566085089252]
[16730939319872750275468906903707539413042652315011]
[94809377245048795150954100921645863754710598436791]
[78639167021187492431995700641917969777599028300699]
[15368713711936614952811305876380278410754449733078]
[40789923115535562561142322423255033685442488917353]
[44889911501440648020369068063960672322193204149535]
[41503128880339536053299340368006977710650566631954]
[81234880673210146739058568557934581403627822703280]
[82616570773948327592232845941706525094512325230608]
[22918802058777319719839450180888072429661980811197]
[77158542502016545090413245809786882778948721859617]
[72107838435069186155435662884062257473692284509516]
[20849603980134001723930671666823555245252804609722]
[53503534226472524250874054075591789781264330331690]




map

need a lambda with cons

let ul list [x:xs,ys] = xs : ys


3:[4]


[3,4] ++ [4,3]

foldl (++) [] [[3,4],[45,4]]


foldl (+) 0 [2,3,4]

:t foldl

map


[33..1]

isPalnindrome 343

let tryIf x = if (x == 0) return True


:t reverseNum


let reverseInt x = (*) (signum x) . read . reverse . show . abs  $ x

show 34


let show' x = "x"
:t show'

let read' "y" = x

filter (/="") $ "x"

filter (/="") ["3"]

filter (<0) [3,4,23,-34]

filter (

filter (/="") "xs


read . reverse . show $ 35


reverseInt 38432


reverseNum 394

reverse [3,4]

reverseNum 394







fibs 3


fastFibs
fastFibs n = fastFibs(n-1) + fastFibs(n-2)


fibs 34





fibs 1 = 1
fibs n = fibs (n-1) + fibs (n-2)
