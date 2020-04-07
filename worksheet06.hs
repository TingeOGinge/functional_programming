{- Week6.hs
 This module illustrates the use of functions as values
-}

import Data.Char
import Data.List

twice :: (Int -> Int) -> Int -> Int
twice f x = f (f x)

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

doubleAll :: [Int] -> [Int]
doubleAll = map (*2)

areDigits :: String -> [Bool]
areDigits = map isDigit

keepPositive :: [Int] -> [Int]
keepPositive = filter (>0)

keepDigits :: String -> String
keepDigits = filter isDigit

addUp :: [Int] -> Int
addUp = foldr (+) 0

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

-------------------------------End of Supplied Code-----------------------------



mult10 :: [Int] -> [Int]
mult10 = map (*10)

onlyLowerCase :: String -> String
onlyLowerCase = filter isLower

orAll :: [Bool] -> Bool
orAll = foldr (||) False

sumSquares :: [Int] -> Int
sumSquares = sum . map(^2)

zeroToTen :: [Int] -> [Int]
zeroToTen = filter (\x -> 0 <= x && x <= 10)

squareRoots :: [Float] -> [Float]
squareRoots =  map(sqrt) . filter (>=0)

countBetween :: Float -> Float -> [Float] -> Int
countBetween x y = length . filter (\z -> x <= z && z <= y)

alwaysPositive :: (Float -> Float) -> [Float] -> Bool
alwaysPositive f = foldr (&&) True . map(\x -> (f x) > 0)

productSquareRoots :: [Float]  -> Float
productSquareRoots = foldr (*) 1 . map(sqrt) . filter(>0)
