import Control.Lens
import Data.Maybe
import Control.Monad

main = do
        input <- readFile "day-2.data"
        print $ process input

data Instruction = L | R | U | D | P deriving (Show, Read)
data Coords = Coords { xPos::Int, yPos::Int } deriving (Show)
data Action = MoveTo { pos::Coords } | Press { pos::Coords } deriving (Show)

inputButtonGrid = [ "    ",
                    "   1 ",
                    "  234 ",
                    " 56789 ",
                    "  ABC ",
                    "   D ",
                    "    " ]

buttonGrid :: [[Maybe Char]]
buttonGrid = map parseRow $ inputButtonGrid
  where
    parseRow = map (\c -> if c == ' ' then Nothing else Just c)

process = map fromJust
          . map buttonAt
          . map pos
          . filter presses
          . foldl walk [MoveTo (Coords 1 3)]
          . map parse

parse '\n' = P
parse c = read [c]

walk :: [Action] -> Instruction -> [Action]
walk as i = as ++ [apply (pos (last as)) i]

apply :: Coords -> Instruction -> Action
apply c P = Press c
apply c L = move c (-1) 0
apply c R = move c 1 0
apply c U = move c 0 (-1)
apply c D = move c 0 1

move :: Coords -> Int -> Int -> Action
move c@(Coords x y) dx dy = MoveTo $ if isNothing next then c else target
  where
    target = Coords (x+dx) (y+dy)
    next = buttonAt target

presses (Press _) = True
presses _ = False

buttonAt :: Coords -> Maybe Char
buttonAt (Coords x y) = do
  maybeButtonRow <- buttonGrid ^? element y
  maybeButton <- join $ maybeButtonRow ^? element x
  return maybeButton
