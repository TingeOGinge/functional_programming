import Data.Char (toUpper, toLower)
import Prelude hiding ((||))

(||) :: Bool -> Bool -> Bool
True  || _ = True
False || p = p

fact :: Int -> Int
fact n
  | n > 0     = n * fact(n - 1)
  | n == 0    = 1
  | otherwise = error "Undefined for negative integers"

fact' :: Int -> Int
fact' n = if n == 0 then 1 else n * fact(n - 1)
