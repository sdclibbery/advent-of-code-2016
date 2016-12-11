{-# LANGUAGE DeriveFunctor #-}

import Text.Parsec
import Text.Parsec.Char

main = do
        input <- readFile "day-7-2.test.data"
--        print $ (length.getSsl) input
        print $ getSsl input

data Component a = Supernet a | Hypernet a deriving (Show, Functor)
data Aba = Aba Char Char deriving (Show)

getSsl = map (map (fmap findAbas))
          .map parseIP
          .lines

parseIP :: String -> [Component String]
parseIP s = right $ parse iPparser "day-7" s
  where
    right (Right x) = x

iPparser :: Parsec String st [Component String]
iPparser = many (supernetParser <|> hypernetParser)
  where
    supernetParser = Supernet <$> text
    hypernetParser = Hypernet <$> ((char '[') *> text <* (char ']'))
    text = many1 $ noneOf "[]"

findAbas :: String -> [Aba]
findAbas = foldl extract [] . triples
  where
    triples s = zip3 (tail $ tail s) (tail s) s
    extract xs (a,b,c) | (a==c && a/=b) = (Aba a b) : xs
    extract xs _ = xs
