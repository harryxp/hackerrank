import Data.List

hello_worlds n = (putStr . intercalate "\n" . replicate n) "Hello World" -- Complete this function

-- This part is related to the Input/Output and can be used as it is
-- Do not modify it
main = do
   n <- readLn :: IO Int
   hello_worlds n
