f :: [Int] -> [Int] -- Fill up this Function
f lst = aux 0 lst
  where
    aux i []     = []
    aux i (x:xs) = if odd i then (x : aux (i+1) xs) else aux (i+1) xs

-- This part deals with the Input and Output and can be used as it is. Do not modify it.
main = do
   inputdata <- getContents
   mapM_ (putStrLn. show). f. map read. lines $ inputdata

