solve :: Double -> Double -- Insert your code here --
solve x = 1 + sum (map calcTerm [1..9])
  where
    calcTerm :: Int -> Double
    calcTerm n = x^n / fromIntegral(fact n)
    fact :: Int -> Int
    fact 1 = 1
    fact n = n * fact (n-1)

main :: IO ()
main = getContents >>= mapM_ print. map solve. map (read::String->Double). tail. words

