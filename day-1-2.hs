import Data.List.Split
import Data.Maybe
import Debug.Trace
import qualified Data.Set as Set

input = "R1, L3, R5, R5, R5, L4, R5, R1, R2, L1, L1, R5, R1, L3, L5, L2, R4, L1, R4, R5, L3, R5, L1, R3, L5, R1, L2, R1, L5, L1, R1, R4, R1, L1, L3, R3, R5, L3, R4, L4, R5, L5, L1, L2, R4, R3, R3, L185, R3, R4, L5, L4, R48, R1, R2, L1, R1, L4, L4, R77, R5, L2, R192, R2, R5, L4, L5, L3, R2, L4, R1, L5, R5, R4, R1, R2, L3, R4, R4, L2, L4, L3, R5, R4, L2, L1, L3, R1, R5, R5, R2, L5, L2, L3, L4, R2, R1, L4, L1, R1, R5, R3, R3, R4, L1, L4, R1, L2, R3, L3, L2, L1, L2, L2, L1, L2, R3, R1, L4, R1, L1, L4, R1, L2, L5, R3, L5, L2, L2, L3, R1, L4, R1, R1, R2, L1, L4, L4, R2, R2, R2, R2, R5, R1, L1, L4, L5, R2, R4, L3, L5, R2, R3, L4, L1, R2, R3, R5, L2, L3, R3, R1, R3"

data Turn = L | R deriving (Show, Read)
data Action = Action {turn :: Turn, dist :: Int} deriving (Show, Read)
data Direction = N | S | E | W deriving (Show, Eq, Ord)
data Pose = Pose {dir :: Direction, xPos :: Int, yPos :: Int} deriving (Show, Eq, Ord)

main = print
        $ directDistance
        $ fromJust
        $ firstDuplicate
        $ foldl applyAction [(Pose N 0 0)]
        $ map parse
        $ splitOn ", "
        $ input

parse :: String -> Action
parse s = Action turn distance
  where
    turn = read $ [head s]
    distance = read $ tail s

applyAction :: [Pose] -> Action -> [Pose]
applyAction ps (Action t d) = ps ++ (walk d $ rotate (last ps) t)

rotate :: Pose -> Turn -> Pose
rotate (Pose d x y) t = Pose (rotateDir d t) x y
  where
    rotateDir N L = W
    rotateDir E L = N
    rotateDir S L = E
    rotateDir W L = S
    rotateDir N R = E
    rotateDir E R = S
    rotateDir S R = W
    rotateDir W R = N

walk :: Int -> Pose -> [Pose]
walk d (Pose N x y) = tail [Pose N x y' | y' <- [y .. y+d]]
walk d (Pose E x y) = tail [Pose E x' y | x' <- [x .. x+d]]
walk d (Pose S x y) = tail [Pose S x (-y') | y' <- [-y .. -y+d]]
walk d (Pose W x y) = tail [Pose W (-x') y | x' <- [-x .. -x+d]]

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate xs = dup' xs Set.empty
  where dup' [] _ = Nothing
        dup' (x:xs) s = if Set.member x s
                           then Just x
                           else dup' xs (Set.insert x s)

directDistance :: Pose -> Int
directDistance (Pose _ x y) = (abs x) + (abs y)
