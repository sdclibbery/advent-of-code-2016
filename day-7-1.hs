import Text.Parsec
import Text.Parsec.Char

main = do
        input <- readFile "day-7.data"
        print $ (length.getTLS) input
--        print $ getTLS input

type Normal = String
type Hypernet = String
data Component = Normal String | Hypernet String deriving (Show)
data TlsStatus = MightHaveTls | DontKnow | DefinitelyDoesntHaveTls deriving (Show)

getTLS = filter isTls
          .map (foldl combineTlsStatus DontKnow)
          .map (map checkTls)
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

checkTls :: Component -> TlsStatus
checkTls (Normal s) | (hasAbba s) = MightHaveTls
checkTls (Hypernet s) | (hasAbba s) = DefinitelyDoesntHaveTls
checkTls _ = DontKnow

hasAbba :: String -> Bool
hasAbba (a:b:c:d:cs)
  | (b==c && a==d && a /=b) = True
  | otherwise = hasAbba (b:c:d:cs)
hasAbba _ = False

combineTlsStatus DefinitelyDoesntHaveTls _ = DefinitelyDoesntHaveTls
combineTlsStatus _ DefinitelyDoesntHaveTls = DefinitelyDoesntHaveTls
combineTlsStatus MightHaveTls _ = MightHaveTls
combineTlsStatus _ MightHaveTls = MightHaveTls
combineTlsStatus _ _ = DontKnow

isTls MightHaveTls = True
isTls _ = False
