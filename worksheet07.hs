
-- Day algebraic type
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
           deriving (Eq,Ord,Show,Read)

-- Alternative definitions of isWeekend function
isWeekend :: Day -> Bool
isWeekend Sat  = True
isWeekend Sun  = True
isWeekend _    = False

isWeekend2 day = day == Sat || day == Sun

isWeekend3 day = day >= Sat

-- Copy of StudentMark type synonym from worksheet 4
data StudentMark = Student String Int
     deriving (Eq,Show)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (Student s1 m1) (Student s2 m2)
    | m1 >= m2          = s1
    | otherwise         = s2

-- Shapes algebraic type
-- data Shape = Circle Float |
--              Rectangle Float Float
--
-- area :: Shape -> Float
-- area (Circle r)      = pi * r * r
-- area (Rectangle h w) = h * w

-- Address algebraic type (note that a constructor can have
-- the same name as the type).
data Address = Address Building String
               deriving (Show)

data Building = Name String |
                Number Int
                deriving (Show)

-- Binary tree algebraic type
data Tree = Null |
     Node Int Tree Tree
     deriving (Eq,Show)

-- Binary tree test data
testTree = Node 20 (Node 3 (Node 12 Null Null) (Node 7 Null Null))
                  (Node 8 (Node 4 (Node 6 Null Null) Null) Null)

-- Binary search tree test data
testSearchTree =  Node 5 (Node 1 Null Null)
                         (Node 8 (Node 7 Null Null) Null)

height :: Tree -> Int
height Null = 0
height (Node _ st1 st2) = 1 + max (height st1) (height st2)

sumValues :: Tree -> Int
sumValues Null = 0
sumValues (Node n st1 st2) = n + sumValues st1 + sumValues st2


-------------------------------End of Supplied Code-----------------------------

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
              deriving (Eq,Ord,Show,Read)

data Season = Spring | Summer | Autumn | Winter deriving (Eq,Ord,Show,Read)

season :: Month -> Season
season month
  | month == Dec || month <= Feb  = Winter
  | month <= May                  = Spring
  | month <= Aug                  = Summer
  | otherwise                     = Autumn

numberOfDays :: Month -> Int -> Int
numberOfDays month year
  | month `elem` [Apr, Jun, Sep, Nov] = 30
  | month == Feb && year `mod` 4 == 0 = 29
  | month == Feb = 28
  | otherwise = 31

data Point = Point Float Float
             deriving (Eq,Show)

data PositionedShape = Circle Point Float |
                       Rectangle Point Float Float
                       deriving (Eq,Show)

move :: PositionedShape -> Float -> Float -> PositionedShape
move (Circle (Point x y) r) dx dy = Circle (Point (x+dx) (x+dy)) r
move (Rectangle (Point x y) h w) dx dy = Rectangle (Point (x+dx) (x+dy)) h w

numberOfNodes :: Tree -> Int
numberOfNodes Null = 0
numberOfNodes (Node n st1 st2) = 1 + numberOfNodes st1 + numberOfNodes st2

isMember :: Int -> Tree -> Bool
isMember x Null = False
isMember x (Node n st1 st2)
  | n == x = True
  | otherwise = False || isMember x st1 || isMember x st2

leaves :: Tree -> [Int]
leaves (Node n st1 st2)
  | st1 /= Null && st2 /= Null = leaves st1 ++ leaves st2
  | st1 /= Null = leaves st1
  | st2 /= Null = leaves st2
  | otherwise = [n]

inOrder :: Tree -> [Int]
inOrder (Node n st1 st2)
  | st1 /= Null && st2 /= Null = inOrder st1 ++ [n] ++ inOrder st2
  | st1 /= Null = inOrder st1 ++ [n]
  | st2 /= Null = [n] ++ inOrder st2
  | otherwise = [n]

insert :: Int -> Tree -> Tree
insert x Null = Node x Null Null
insert x (Node n st1 st2)
  | x < n && st1 /= Null = (Node n (insert x st1) st2)
  | x < n && st1 == Null = (Node n (Node x Null Null) st2)
  | x > n && st2 /= Null = (Node n st1 (insert x st2))
  | x > n && st2 == Null = (Node n st1 (Node x Null Null))
  | otherwise = (Node x st1 st2)

listToSearchTree :: [Int] -> Tree
listToSearchTree = foldr (insert) Null . reverse

binaryTreeSort :: [Int] -> [Int]
binaryTreeSort = inOrder . listToSearchTree
