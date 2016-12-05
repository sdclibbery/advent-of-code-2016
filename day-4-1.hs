import Data.Char
import Data.List
import Debug.Trace

main = do
        input <- readFile "day-4.data"
        print $ countRealRooms input

data Room = Room { name::String, checksum::String } deriving (Show)

countRealRooms = id
                  .length
                  -- .map (calcChecksum.name)
                  .filter isValidRoom
                  .map parseRoom
                  .lines

parseRoom :: String -> Room
parseRoom s = Room name checksum
  where
    name = filter isLetter $ takeWhile (/= '[') s
    checksum = reverse $ filter isLetter $ takeWhile (/= '[') $ reverse s

isValidRoom :: Room -> Bool
isValidRoom (Room n c) = (c == calcChecksum n)

calcChecksum :: String -> String
calcChecksum = map fst . take 5 . sortBy compareByCountThenAlphabetically . map counts . group . sort
  where
    counts cs = (head cs, length cs)

compareByCountThenAlphabetically :: (Char, Int) -> (Char, Int) -> Ordering
compareByCountThenAlphabetically (ln, lc) (rn, rc)
  | (lc /= rc) = compare rc lc
  | (lc == rc) = compare ln rn
