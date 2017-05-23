import Text.ParserCombinators.ReadP
import Text.Printf

main :: IO ()
main = do
    line <- getLine
    case readP_to_S twelveP line of
        [(timestamp,"")] -> putStrLn timestamp
        otherwise -> error "cannot parse"

twelveP :: ReadP String
twelveP = do
    hour <- count 2 (satisfy (\char -> char >= '0' && char <= '9'))
    satisfy (==':')
    minute <- count 2 (satisfy (\char -> char >= '0' && char <= '9'))
    satisfy (==':')
    second <- count 2 (satisfy (\char -> char >= '0' && char <= '9'))
    ampm <- count 2 (satisfy (\char -> char >= 'A' && char <= 'Z'))
    let hour' = buildHour ampm hour
    return (printf "%s:%s:%s" hour' minute second)

buildHour "AM" "12" = "00"
buildHour "PM" "12" = "12"
buildHour "AM" hour = hour
buildHour "PM" hour = show (12 + read hour)
buildHour _ _ = error "something you haven't thought about"
