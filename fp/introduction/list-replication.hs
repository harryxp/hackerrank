f :: Int -> [Int] -> [Int] -- Complete this function
f n []     = []
f n (x:xs) = (replicate n x) ++ f n xs

-- This part handles the Input and Output and can be used as it is. Do not modify this part.
main :: IO ()
main = getContents >>=
  mapM_ print. (\(n:arr) -> f n arr). map read. words

