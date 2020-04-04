{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)

-- Definitions of the prelude functions fst and snd

fst (x,_)       = x
snd (_,y)       = y

-- Definitions of the prelude functions head and tail

head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []

-------------------------------End of Supplied Code-----------------------------

-----------------------------------List Patterns--------------------------------

headPlusOne :: [Int] -> Int
headPlusOne [] = 0
headPlusOne (x:xs) = x + 1

duplicateHead :: [a] -> [a]
duplicateHead [] = []
duplicateHead (x:xs) = x:x:xs

rotate :: [a] -> [a]
rotate (x:y:ys) = y:x:ys
rotate n = n

-----------------------------------List Recursion-------------------------------

listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1 + listLength xs

multAll :: [Int] -> Int
multAll [] = 1
multAll (x:xs) = x * multAll xs

andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs

countElems :: Int -> [Int] -> Int
countElems n [] = 0
countElems n (x:xs) = if n == x then 1 + countElems n xs else countElems n xs

removeAll :: Int -> [Int] -> [Int]
removeAll n [] = []
removeAll n (x:xs) = if n == x then removeAll n xs else x : removeAll n xs

type StudentMark = (String, Int)

testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("John", 41), ("Sue", 71)]

listMarks :: String -> [StudentMark] -> [Int]
listMarks n [] = []
listMarks n ((st, mk):xs) = if n == st then mk : listMarks n xs else listMarks n xs

sorted :: [Int] -> Bool
sorted [] = True
sorted (x:y:ys) = if x <= y then True && sorted (y:ys) else False && sorted []
sorted (x:xs) = True

prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = if x == y then True && prefix xs ys else False

subSequence :: [Int] -> [Int] -> Bool
subSequence [] _ = True
subSequence _ [] = False
subSequence x (y:ys) = prefix x (y:ys) || subSequence x ys
