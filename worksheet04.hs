import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2)
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stmks = [ mk | (st,mk) <- stmks ]

pass :: [StudentMark] -> [String]
pass stmks = [ st | (st,mk) <- stmks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y
    | x <= y            = (x,y)
    | otherwise         = (y,x)

-------------------------------End of Supplied Code----------------

-------------------------------------Tuples------------------------

sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = ((x + y), (x - y))

grade :: StudentMark -> Char
grade (name, mark)
  | mark >= 70 = 'A'
  | mark >= 60 = 'B'
  | mark >= 50 = 'C'
  | mark >= 40 = 'D'
  | otherwise  = 'F'

capMark :: StudentMark -> StudentMark
capMark (st, mk)
  | mk > 40 = (st, 40)
  | otherwise = (st, mk)

-----------------------------------Lists and Strings---------------

firstNumbers :: Int -> [Int]
firstNumbers n = [1..n]

firstSquares :: Int -> [Int]
firstSquares n = [x ^ 2 | x <- firstNumbers n]

capitalise :: String -> String
capitalise n = [toUpper x | x <- n]

onlyDigits :: String -> String
onlyDigits n = [x | x <- n, isDigit x]

capMarks :: [StudentMark] -> [StudentMark]
capMarks n = [capMark (st, mk) | (st, mk) <- n]

gradeStudents :: [StudentMark] -> [(String, Char)]
gradeStudents n = [(st, grade (st, mk)) | (st, mk) <- n]

duplicate :: String -> Int -> String
duplicate s n = if n == 1 then s else s ++ duplicate s (n - 1)

divisors :: Int -> [Int]
divisors n = [x  | x <- [1..n], (n `mod` x == 0)]

isPrime :: Int -> Bool
isPrime n = length (divisors n) == 2

split :: [(a, b)] -> ([a], [b])
split n = ([a | (a, b) <- n], [b | (a, b) <- n])
