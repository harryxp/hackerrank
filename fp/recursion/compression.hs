import Data.List (group, intercalate)

main :: IO ()
main = getLine >>= \ln -> putStrLn $ compress ln

compress :: String -> String
compress s = (intercalate "" . map (\(x:xs) -> (x: if null xs then [] else (show(length xs+1)))) . group) s

