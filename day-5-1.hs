import Data.Hash.MD5
import Data.List

--input = "abc"
input = "ojvtpuvg"

main = print $ password input

password :: String -> String
password s = (map (!! 5) . take 8 . filter (isPrefixOf "00000") . map generateHash . map (\v -> s++v) . map show) [1..]

generateHash = md5s . Str
