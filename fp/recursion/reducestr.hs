import Data.Set (Set, empty, insert, member)

main :: IO ()
main = getLine >>= putStrLn . reduceStr

reduceStr :: String -> String
reduceStr s = reduceStr' empty s

reduceStr' :: Set Char -> String -> String
reduceStr' _ [] = ""
reduceStr' chars (x:xs) = if member x chars then reduceStr' chars xs else (x:reduceStr' (insert x chars) xs)

-- nub in Data.List does just this
