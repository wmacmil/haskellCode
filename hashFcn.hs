
data AreaCode = AreaCode Int Int Int deriving (Show)


let reno = AreaCode 7 7 5

reno

:{
let reno :: AreaCode
    reno = 7 7 5
:}

data PhoneNumber = AreaCode FirstThree LastFour

import Data.Hashable

map hash ['a'..'z']

:t 'b'

'b' : 'c' : []

map hash $ map (: ['b'] ) ['a'..'z']

hash "foo"
hash "boo"

hashWithSalt 3 "foo"
hashWithSalt 4 "foo"

hashWithSalt (hashWithSalt 3 "foo")

map (\x -> hashWithSalt x "fo") [1..10]

hash [1,2]
hash [1,3]

hash 4

hash "sokeo"
