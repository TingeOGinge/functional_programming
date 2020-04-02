absolute :: Int -> Int
absolute x
  | x < 0     = (-x)
  | otherwise = x

sign :: Int -> Int
sign x
  | x < 0     = (-1)
  | x == 0    = 0
  | otherwise = 1

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
  | x == y && x == z            = 3
  | x == y || x == z || z == y  = 2
  | otherwise                   = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = squareDiag x + squareDiag y + squareDiag z
                            where
                            squareDiag x = x * sqrt(2)

taxiFare :: Float -> Float
taxiFare x = 2.20 + distCalc x
  where
  distCalc y
    | y <= 10 = firstRate y
    | otherwise = firstRate 10 + secondRate (y - 10)
  firstRate i   = 0.5 * i
  secondRate j  = 0.3 * j

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z = (compAvg x) + (compAvg y) + (compAvg z)
  where
    avg = (x + y + z) `div` 3
    compAvg j = if j > avg then 1 else 0


validDate :: Int -> Int -> Bool
validDate day month
  | month < 0 || day < 0                                = False
  | month `elem` [1, 3, 5, 7, 8, 10, 12] && day <= 31   = True
  | month `elem` [4, 6, 9, 11] && day <= 30             = True
  | month == 2 && day <= 28                             = True
  | otherwise                                           = False

daysInMonth :: Int -> Int -> Int
daysInMonth month year
  | month < 0 || month > 12               = (-1)
  | month `elem` [1, 3, 5, 7, 8, 10, 12]  = 31
  | month `elem` [4, 6, 9, 11]            = 30
  | month == 2 && year `mod` 4 == 0       = 29
  | otherwise                             = 28
