import Text.Parsec
import Text.Parsec.Char

main = do
        input <- readFile "day-7.test.data"
--        print $ (length.getTLS) input
        print $ getTLS input

type Normal = String
type Hypernet = String
data Component = Normal String | Hypernet String deriving (Show)

getTLS = map (map (hasAbba.getText))
          .map parseIP
          .lines

parseIP :: String -> [Component]
parseIP s = right $ parse iPparser "day-7" s
  where
    right (Right x) = x

iPparser :: Parsec String st [Component]
iPparser = many (normalParser <|> hypernetParser)
  where
    normalParser = Normal <$> text
    hypernetParser = Hypernet <$> ((char '[') *> text <* (char ']'))
    text = many1 $ noneOf "[]"

getText (Normal s) = s
getText (Hypernet s) = s

hasAbba :: String -> Bool
hasAbba (a:b:c:d:cs)
  | (b==c && a==d && a /=b) = True
  | otherwise = hasAbba (b:c:d:cs)
hasAbba _ = False
