import Control.Monad (replicateM_)

hello_worlds :: Int -> IO ()
hello_worlds n = replicateM_ n (putStrLn "Hello World") -- Complete this function

-- This part is related to the Input/Output and can be used as it is
-- Do not modify it
main = do
   n <- readLn :: IO Int
   hello_worlds n
