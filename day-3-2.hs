import Text.ParserCombinators.Parsec
import Data.Char

--main = print $ countImpossibleTriangles "5  10 25"
main = do
        input <- readFile "day-3-1.data"
        print $ countImpossibleTriangles input

countImpossibleTriangles = length
                            . filter validTri
                            . columnToTriangles
                            . concat
                            . foldl addToColumns [[], [], []]
                            . lines

type Columns = [[Int]]
type Triangle = (Int,Int,Int)

addToColumns :: Columns -> String -> Columns
addToColumns [c0, c1, c2] s = [sides!!0 : c0, sides!!1 : c1, sides!!2 : c2]
  where
    sides = foldl parseChar [0] s

parseChar :: [Int] -> Char -> [Int]
parseChar (i:is) c = if isDigit c
                      then ((i*10 + (digitToInt c)):is)
                      else (if i == 0 then (i:is) else (0:i:is))

columnToTriangles :: [Int] -> [Triangle]
columnToTriangles [] = []
columnToTriangles (s1:s2:s3:ss) = (s1,s2,s3) : (columnToTriangles ss)

validTri :: Triangle -> Bool
validTri (a,b,c) = (a+b > c) && (b+c > a) && (a+c > b)
