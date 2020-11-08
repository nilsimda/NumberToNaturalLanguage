module Exercise01 where

import Test.QuickCheck
import Text.Printf

{-H1.1a)-}
myPair :: Integer -> Integer -> Integer
myPair x y = 2^y * (2 * x + 1) - 1

{-H1.1b)-}
mySnd :: Integer -> Integer
mySnd y
    | even y = 0
    | odd y = 1 + mySnd (y `div` 2)


{-H1.1c)-}
myFst :: Integer -> Integer
myFst x = ((x+1) `div` (2 ^ mySnd x) -1) `div` 2

{-H1.1d)-}
prop_myPair :: Integer -> Integer -> Integer -> Property
prop_myPair p x y = 
    p >= 0 && x>=0 && y>=0  ==> property (myPair x y == p)

{-H2-}
digitToEo :: Integer -> String
digitToEo 0 = "nul"
digitToEo 1 = "unu"
digitToEo 2 = "du"
digitToEo 3 = "tri"
digitToEo 4 = "kvar"
digitToEo 5 = "kvin"
digitToEo 6 = "ses"
digitToEo 7 = "sep"
digitToEo 8 = "ok"
digitToEo 9 = "nau"

{-WETT-}

numberToEo :: Integer -> String
numberToEo n = 
        if last res == ' ' then init res else res
      where res = digitToEoHelper (mod (div x 100000) 10) "cent " 
                    ++ digitToEoHelper (mod (div x 10000) 10) "dek " 
                    ++ digitToEoHelper (mod (div x 1000) 10)  " "
                    ++ digitToEoHelper x "mil"
                    ++ digitToEoHelper (mod (div x 100) 10) "cent "
                    ++ digitToEoHelper (mod (div x 10) 10) "dek "
                    ++ digitToEoHelper (mod x 10) ""
                    where x = read (printf "%06d" n)


digitToEoHelper:: Integer -> String -> String
digitToEoHelper x "mil" = if x < 1000 then "" else "mil "
digitToEoHelper 0 s = ""
digitToEoHelper 1 s = if s == "" || s == " " then "unu " else s
digitToEoHelper x s = digitToEo x ++ s  

{-TTEW-}
