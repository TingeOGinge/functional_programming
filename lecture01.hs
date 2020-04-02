square :: Int -> Int
square n = n * n

twiceSum :: Int -> Int -> Int
twiceSum x y = 2 * (x + y)

exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)
