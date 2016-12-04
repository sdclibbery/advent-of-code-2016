import Text.ParserCombinators.Parsec
import Data.Char

--main = print $ countImpossibleTriangles "5  10 25"
main = do
        input <- readFile "day-3-1.data"
        print $ countImpossibleTriangles input

countImpossibleTriangles = length
                            . filter validTri
                            . map parseTri
                            . lines

type Triangle = (Int,Int,Int)

parseTri :: String -> Triangle
parseTri s = (sides!!0, sides!!1, sides!!2)
  where
    sides = foldl parseChar [0] s

parseChar :: [Int] -> Char -> [Int]
parseChar (i:is) c = if isDigit c
                      then ((i*10 + (digitToInt c)):is)
                      else (if i == 0 then (i:is) else (0:i:is))

validTri :: Triangle -> Bool
validTri (a,b,c) = (a+b > c) && (b+c > a) && (a+c > b)
