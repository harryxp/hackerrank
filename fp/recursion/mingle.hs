import Data.List (intercalate)

main :: IO ()
main =
  getLine >>=
  \p -> getLine >>=
  \q -> putStrLn $ mingle p q

mingle [] [] = ""
mingle (x:xs) (y:ys) = (x:(y:mingle xs ys))
mingle _ _ = ""   -- should not happen

-- mingle_slow p q = foldl (\accumed (a,b) -> intercalate "" [accumed, [a], [b]]) "" (zip p q)
