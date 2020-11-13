module Exercise01 where

import Test.QuickCheck ( (==>), Property, Testable(property) )
import qualified Data.Text as T

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
numberToEo 0 = "nul" 
numberToEo n =  T.unpack (T.strip (T.pack (concat [helper (mod x 1000) p ++ powers p ++ " "| 
                            (x,p) <- zip (splitTo3 n) (reverse [0 .. toInteger (length (splitTo3 n))-1])])))

splitTo3 :: Integer -> [Integer]
splitTo3 0 = []
splitTo3 n = splitTo3 (div n 1000) ++ [mod n 1000]

helper:: Integer -> Integer -> String
helper n p
    | n == 0 = ""
    | n < 10 = if p == 1 then digitToEoFilter1 n ++ " " else digitToEo n ++ " "
    | n < 100 = digitToEoFilter1 (div n 10) ++ "dek " ++ helper (mod n 10) 0
    | otherwise = digitToEoFilter1 (div n 100) ++ "cent " ++ helper (mod n 100) 0

digitToEoFilter1 :: Integer -> String
digitToEoFilter1 1 = ""
digitToEoFilter1 n = digitToEo n

powers :: Integer -> String
powers 0 = ""
powers 1 = "mil"
powers 2 = "milionoj"
powers 3 = "miliardoj"
powers n
    | even n = numberToEo n ++ "ilionoj"
    | otherwise = numberToEo n ++ "iliardoj"

{-TTEW-}

