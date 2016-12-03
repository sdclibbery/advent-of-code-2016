import Prelude hiding (Left, Right)

main = do
        input <- readFile "day-2.test.data"
        print $ process input

data Instruction = Left | Right | Up | Down | Press deriving (Show, Read)

process = map parse

parse '\n' = Press
parse 'L' = Left
parse 'R' = Right
parse 'U' = Up
parse 'D' = Down
