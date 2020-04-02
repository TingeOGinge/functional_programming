maxVal :: Int -> Int -> Int
maxVal x y
  | x >= y    = x
  | otherwise = y

maxThree :: Int -> Int -> Int -> Int
maxThree x y z
  | x >= y && x >= z  = x
  | y >= z            = y
  | otherwise         = z

distance :: Float -> Float -> Float -> Float -> Float
distance x1 x2 y1 y2 = sqrt(dxSq + dySq)
                        where
                        dx = x1 - x2
                        dy = y1 - y2
                        dxSq = dx ^ 2
                        dySq = dy ^ 2

sumPosCubes :: Float -> Float -> Float -> Float
sumPosCubes x y z = posCube x + posCube y + posCube z
                    where
                    posCube a = abs (a ^ 3)

numMultsOf10OG :: Int -> Int -> Int
numMultsOf10OG a b
  | multOf10 a && multOf10 b = 2
  | multOf10 a || multOf10 b = 1
  | otherwise                = 0
  where
  multOf10 x = x `mod` 10 == 0

numMultsOf10New :: Int -> Int -> Int
numMultsOf10New a b = multOf10 a + multOf10 b
                      where
                      multOf10 a = if a `mod` 10 == 0 then 1 else 0
