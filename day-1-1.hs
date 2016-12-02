import Data.List.Split

input = "R1, L3, R5, R5, R5, L4, R5, R1, R2, L1, L1, R5, R1, L3, L5, L2, R4, L1, R4, R5, L3, R5, L1, R3, L5, R1, L2, R1, L5, L1, R1, R4, R1, L1, L3, R3, R5, L3, R4, L4, R5, L5, L1, L2, R4, R3, R3, L185, R3, R4, L5, L4, R48, R1, R2, L1, R1, L4, L4, R77, R5, L2, R192, R2, R5, L4, L5, L3, R2, L4, R1, L5, R5, R4, R1, R2, L3, R4, R4, L2, L4, L3, R5, R4, L2, L1, L3, R1, R5, R5, R2, L5, L2, L3, L4, R2, R1, L4, L1, R1, R5, R3, R3, R4, L1, L4, R1, L2, R3, L3, L2, L1, L2, L2, L1, L2, R3, R1, L4, R1, L1, L4, R1, L2, L5, R3, L5, L2, L2, L3, R1, L4, R1, R1, R2, L1, L4, L4, R2, R2, R2, R2, R5, R1, L1, L4, L5, R2, R4, L3, L5, R2, R3, L4, L1, R2, R3, R5, L2, L3, R3, R1, R3"

data Turn = L | R deriving (Show, Read)
data Action = Action {turn :: Turn, dist :: Int} deriving (Show, Read)
data Direction = N | S | E | W deriving (Show, Read)
data Pose = Pose {dir :: Direction, xPos :: Int, yPos :: Int} deriving (Show, Read)

main = print
        $ shortestDistance
        $ foldr combine (Pose N 0 0)
        $ map parse
        $ splitOn ", "
        $ input

parse :: String -> Action
parse s = Action turn distance
  where
    turn = read $ [head s]
    distance = read $ tail s

combine :: Action -> Pose -> Pose
combine (Action t d) p = move d $ rotate p t

rotate :: Pose -> Turn -> Pose
rotate (Pose d x y) t = Pose (rotateDir d t) x y

rotateDir :: Direction -> Turn -> Direction
rotateDir N L = W
rotateDir E L = N
rotateDir S L = E
rotateDir W L = S
rotateDir N R = E
rotateDir E R = S
rotateDir S R = W
rotateDir W R = N

move :: Int -> Pose -> Pose
move d (Pose N x y) = Pose N x (y+d)
move d (Pose E x y) = Pose E (x+d) y
move d (Pose S x y) = Pose S x (y-d)
move d (Pose W x y) = Pose W (x-d) y

shortestDistance :: Pose -> Int
shortestDistance (Pose _ x y) = (abs x) + (abs y)
