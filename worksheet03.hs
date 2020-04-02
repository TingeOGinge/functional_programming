import Prelude hiding ((&&))
infixr 3 &&

(&&) :: Bool -> Bool -> Bool
-- True && True = True
-- True && False = False
-- False && True = False
-- False && False = False

True && True = True
_ && _ = False

-- True && p = p
-- _ && _ = False

exOr :: Bool -> Bool -> Bool
exOr True False = True
exOr False True = True
exOr _ _        = False

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True x y = x
ifThenElse False x y = y

sumNumbers :: Int -> Int
sumNumbers n = if n == 1 then n else n + sumNumbers(n - 1)

sumSquares :: Int -> Int
sumSquares n = if n == 1 then n else n ^ 2 + sumSquares(n - 1)

power :: Int -> Int -> Int
power base n = if n == 1 then base else base * power base (n - 1)

sumFromTo :: Int -> Int -> Int
sumFromTo x y
