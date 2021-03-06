import Prelude hiding ((&&), gcd)
infixr 3 &&

(&&) :: Bool -> Bool -> Bool
-- -- True && True = True
-- -- True && False = False
-- -- False && True = False
-- -- False && False = False

True && True = True
_ && _ = False

-- -- True && p = p
-- -- _ && _ = False

exOr :: Bool -> Bool -> Bool
exOr True False = True
exOr False True = True
exOr _ _        = False

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True x y = x
ifThenElse False x y = y

daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth 4 = 30
daysInMonth 6 = 30
daysInMonth 9 = 30
daysInMonth 11 = 30
daysInMonth _ = 31

validDate :: Int -> Int -> Bool
validDate day month = day <= daysInMonth month

sumNumbers :: Int -> Int
sumNumbers n = if n == 1 then n else n + sumNumbers(n - 1)

sumSquares :: Int -> Int
sumSquares n = if n == 1 then n else n ^ 2 + sumSquares(n - 1)

power :: Int -> Int -> Int
power base n = if n == 1 then base else base * power base (n - 1)

sumFromTo :: Int -> Int -> Int
sumFromTo x y
  | x > y     = 0
  | x == y    = x
  | otherwise = x + sumFromTo (x + 1) y

gcd :: Int -> Int -> Int
gcd a b
  | a == 0            = b
  | b == 0 || a == b  = a
  | a > b             = gcd (a - b) b
  | otherwise         = gcd a (b - a)

intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

findRoot :: Int -> Int -> Int
findRoot n s = if (s ^ 2) > n then findRoot n (s - 1) else s
