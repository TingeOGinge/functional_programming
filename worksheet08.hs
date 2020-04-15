import Text.Printf

helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

displayFile :: IO ()
displayFile = do
    putStr "Enter the filename: "
    name <- getLine
    contents <- readFile name
    putStr contents

getInt :: IO Int
getInt = do
    str <- getLine
    return (read str :: Int)

isPalindrome :: String -> String
isPalindrome str
   | str == reverse str  = str ++ " is a palindrome"
   | otherwise           = str ++ " is not a palindrome"

pal :: IO ()
pal = do
    line <- getLine
    let response = isPalindrome line
    putStrLn response

palLines :: IO ()
palLines = do
    putStr "Enter a line: "
    str <- getLine
    if str == "" then
        return ()
    else do
        putStrLn (isPalindrome str)
        palLines

-- For exercise 6
fahrenheit2Celsius :: Float -> Float
fahrenheit2Celsius f = (f - 32) * 5 / 9

celsius2Fahrenheit :: Float -> Float
celsius2Fahrenheit c = c * 9 / 5 + 32

-------------------------------End of Supplied Code-----------------------------

greeting :: IO ()
greeting = do
  input <- getLine
  putStrLn ("Hello, " ++ input)

addTwoNumbers :: IO ()
addTwoNumbers = do
  num1 <- getInt
  num2 <- getInt
  putStrLn(show (num1 + num2))

copyProgram :: IO ()
copyProgram = do
  original <- readFile "pom.txt"
  newFilename <- getLine
  writeFile newFilename original

buildList :: [String] -> IO ()
buildList list = do
  putStr "Enter a line: "
  newItem <- getLine
  if newItem == "" then
      return ()
    else do
      let newList = list ++ [newItem]
      putStr (printf "List is now: %s\n" (show newList))
      buildList newList

listBuilder :: IO ()
listBuilder = buildList []

addNum :: Int -> Int -> IO Int
addNum 0 acc = return acc
addNum n acc = do
  putStr "Enter your next value: "
  x <- getInt
  addNum(n -1) (acc + x)

sumNIntegers :: IO ()
sumNIntegers = do
  putStr "How many numbers would you like to sum? "
  n <- getInt
  total <- addNum n 0
  putStrLn (printf "The sum of all your integers = %d" total)
