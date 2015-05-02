import Control.Monad (replicateM)

main :: IO ()
main =  handleInput >>= handleOutput . map (show . fullOfColors)

handleInput :: IO [String]
handleInput =  getLine >>= \numS -> let num = read numS in replicateM num getLine

fullOfColors :: String -> Bool
fullOfColors =  fullOfColors' 0 0 0 0

fullOfColors'                            :: Int -> Int -> Int -> Int -> String -> Bool
fullOfColors' numR numG numY numB []     =  numR == numG && numY == numB
fullOfColors' numR numG numY numB (x:xs)
  | x == 'R'  = abs (numR+1 - numG) <= 1 && fullOfColors' (numR+1) numG     numY     numB     xs
  | x == 'G'  = abs (numG+1 - numR) <= 1 && fullOfColors' numR     (numG+1) numY     numB     xs
  | x == 'Y'  = abs (numY+1 - numB) <= 1 && fullOfColors' numR     numG     (numY+1) numB     xs
  | x == 'B'  = abs (numB+1 - numY) <= 1 && fullOfColors' numR     numG     numY     (numB+1) xs
  | otherwise = error "invalid input"

handleOutput :: [String] -> IO ()
handleOutput =  \xs -> mapM putStrLn xs >> return ()

