import Data.Char
import Data.List
import Debug.Trace

main = do
        input <- readFile "day-4.data"
        print $ getNorthPoleRoom input

data Room = Room { name::String, sector::Int, checksum::String } deriving (Show)
data PlainRoom = PlainRoom { plainName::String, plainSector::Int } deriving (Show)

getNorthPoleRoom = id
                  .map plainSector
                  .filter (\r -> plainName r == "northpoleobjectstorage")
                  .map decrypt
                  .filter isValidRoom
                  .map parseRoom
                  .lines

parseRoom :: String -> Room
parseRoom s = Room name sector checksum
  where
    name = filter isLetter $ takeWhile (/= '[') s
    sector = read $ filter isDigit s
    checksum = reverse $ filter isLetter $ takeWhile (/= '[') $ reverse s

isValidRoom :: Room -> Bool
isValidRoom (Room n _ c) = (c == calcChecksum n)

calcChecksum :: String -> String
calcChecksum = map fst . take 5 . sortBy compareByCountThenAlphabetically . map counts . group . sort
  where
    counts cs = (head cs, length cs)

compareByCountThenAlphabetically :: (Char, Int) -> (Char, Int) -> Ordering
compareByCountThenAlphabetically (ln, lc) (rn, rc)
  | (lc /= rc) = compare rc lc
  | (lc == rc) = compare ln rn

decrypt :: Room -> PlainRoom
decrypt (Room name sector _) = PlainRoom (map (rotateChar sector) name) sector

rotateChar :: Int -> Char -> Char
rotateChar r = toChar . rotateBy . fromChar
  where
    fromChar c = (ord c) - (ord 'a')
    rotateBy n = (n + r) `mod` 26
    toChar c = chr $ c + (ord 'a')
