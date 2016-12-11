import Text.Parsec
import Text.Parsec.Char

main = do
        input <- readFile "day-7-2.test.data"
--        print $ (length.getSsl) input
        print $ getSsl input

data Component = Supernet String | Hypernet String deriving (Show)

getSsl = map parseIP
          .lines

parseIP :: String -> [Component]
parseIP s = right $ parse iPparser "day-7" s
  where
    right (Right x) = x

iPparser :: Parsec String st [Component]
iPparser = many (supernetParser <|> hypernetParser)
  where
    supernetParser = Supernet <$> text
    hypernetParser = Hypernet <$> ((char '[') *> text <* (char ']'))
    text = many1 $ noneOf "[]"
