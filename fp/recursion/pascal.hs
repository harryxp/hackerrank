import Data.List (intercalate)

main :: IO ()
main = getLine >>= \ln -> (putStr . pascal) (read ln)

pascal :: Int -> String
pascal k = intercalate "\n" $ map current_line_str [0..k-1]
  where
    current_line_str n = intercalate " " $ map (\r -> show (factorial n `div` (factorial r * factorial (n-r)))) [0..n]
    factorial n = product [1..n]
