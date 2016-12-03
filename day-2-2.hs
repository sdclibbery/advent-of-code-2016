
main = do
        input <- readFile "day-2.test.data"
        print $ process input

data Instruction = L | R | U | D | P deriving (Show, Read)
data Coords = Coords { xPos::Int, yPos::Int } deriving (Show)
data Action = MoveTo Coords | Press Coords deriving (Show)

process = concat . map button . filter presses . foldl walk [MoveTo (Coords 3 3)] . map parse

parse '\n' = P
parse c = read [c]

walk :: [Action] -> Instruction -> [Action]
walk as i = as ++ [apply (pos (last as)) i]

pos (MoveTo c) = c
pos (Press c) = c

apply :: Coords -> Instruction -> Action
apply c P = Press c
apply c _ = MoveTo c -- WriteMe!

presses (Press _) = True
presses _ = False

button (Press (Coords 3 1)) = "1"
button (Press (Coords 2 2)) = "2"
button (Press (Coords 3 2)) = "3"
button (Press (Coords 4 2)) = "4"
button (Press (Coords 1 3)) = "5"
button (Press (Coords 2 3)) = "6"
button (Press (Coords 3 3)) = "7"
button (Press (Coords 4 3)) = "8"
button (Press (Coords 5 3)) = "9"
button (Press (Coords 2 4)) = "A"
button (Press (Coords 3 4)) = "B"
button (Press (Coords 4 4)) = "C"
button (Press (Coords 3 5)) = "D"
button _ = "?"
