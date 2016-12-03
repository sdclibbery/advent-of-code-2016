
main = do
        input <- readFile "day-2.test.data"
        print $ process input

data Instruction = L | R | U | D | P deriving (Show, Read)
data Coords = Coords { xPos::Int, yPos::Int } deriving (Show)
data Action = MoveTo Coords | Press Coords deriving (Show)

process = concat . map show . map button . filter presses . foldl walk [MoveTo (Coords 2 2)] . map parse

parse '\n' = P
parse c = read [c]

walk :: [Action] -> Instruction -> [Action]
walk as i = as ++ [apply (pos (last as)) i]

pos (MoveTo c) = c
pos (Press c) = c

apply c P = Press c
apply (Coords 1 y) L = MoveTo (Coords 1 y)
apply (Coords x y) L = MoveTo (Coords (x-1) y)
apply (Coords 3 y) R = MoveTo (Coords 3 y)
apply (Coords x y) R = MoveTo (Coords (x+1) y)
apply (Coords x 3) D = MoveTo (Coords x 3)
apply (Coords x y) D = MoveTo (Coords x (y+1))
apply (Coords x 1) U = MoveTo (Coords x 1)
apply (Coords x y) U = MoveTo (Coords x (y-1))

presses (Press _) = True
presses _ = False

button (Press (Coords 1 1)) = 1
button (Press (Coords 2 1)) = 2
button (Press (Coords 3 1)) = 3
button (Press (Coords 1 2)) = 4
button (Press (Coords 2 2)) = 5
button (Press (Coords 3 2)) = 6
button (Press (Coords 1 3)) = 7
button (Press (Coords 2 3)) = 8
button (Press (Coords 3 3)) = 9
