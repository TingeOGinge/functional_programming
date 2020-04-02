timesTen :: Int -> Int
timesTen n = n * 10

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

areaOfCircle :: Float -> Float
areaOfCircle radius = pi * radius ** 2

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder radius height = areaOfCircle radius * height

distance :: Float -> Float -> Float -> Float -> Float
distance y1 y2 x1 x2 = sqrt ((y1 - y2) ** 2 + (x1 - x2) ** 2)

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && y /= z && x /= z

divisibleBy :: Int -> Int -> Bool
divisibleBy x y = x `mod` y == 0

isEven :: Int -> Bool
isEven n = divisibleBy n 2

averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (x + y + z) / 3

absolute :: Int -> Int
absolute x = if x < 0 then (-x) else x
