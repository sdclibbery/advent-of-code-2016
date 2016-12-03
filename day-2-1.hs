
main = do
        input <- readFile "day-2.test.data"
        print $ process input

process = lines
