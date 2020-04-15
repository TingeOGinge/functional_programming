import Text.Printf

addWord :: String -> [String] -> [String]
addWord x list = list ++ [x]

wordsToString :: [String] -> String
wordsToString = unlines

wordsOfLength :: Int -> [String] -> [String]
wordsOfLength n = filter(\x -> length x == n)

getInt :: IO Int
getInt = do
    str <- getLine
    return (read str :: Int)

--------------------------------------------------------------------------------

buildList :: [String] -> IO [String]
buildList list = do
  putStr "Enter a line: "
  newItem <- getLine
  if newItem == "" then
      return list
    else do
      let newList = list ++ [newItem]
      putStr (wordsToString newList)
      buildList newList


option :: Int -> [String] -> IO [String]
option 1 list = do
  newList <- buildList list
  return newList

option 2 list = do
  putStr (printf "The list currently contains \n%s\n" (wordsToString list))
  return list

option 3 list = do
  putStr "Display words of length: "
  l <- getInt
  let newList = wordsOfLength l list
  putStr (printf "The list currently contains the following items with length == %d\n\n%s\n" l (wordsToString newList))
  return list

menu :: [String] -> IO [String]
menu list = do
  putStr "Options: \n0 = Exit \n1 = Add a word to the list \n2 = Display all \n3 = Display all of length x\n\n"
  choice <- getInt
  if choice == 0 then
    return list
    else do
      newList <- option choice list
      menu newList

main :: IO ()
main = do
  raw <- readFile "words.txt"
  let list = (read raw :: [String])
  newList <- menu list
  writeFile "words.txt" (show newList)
