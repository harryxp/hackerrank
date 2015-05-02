import Control.Monad (replicateM)

main :: IO ()
main = handleInput >>= (putStrLn . show . solve)

handleInput :: IO (Int, Int)
handleInput =  replicateM 2 getLine >>= \[sumLine, powerLine] -> return (read sumLine, read powerLine)

solve                    :: (Int, Int) -> Int
solve (targetSum, power) =
  let upperBound :: Int
      upperBound =  (until (\n -> n^power > targetSum) (+1) 1) - 1
      candidates =  map (^power) [1..upperBound]
  in solve' targetSum candidates

-- this reduces targetSum and candidates at the same time so it has two bases cases
solve'                  :: Int -> [Int] -> Int
solve' 0         _      =  1
solve' _         []     =  0
solve' targetSum (x:xs)
  | targetSum < 0 = 0
  | otherwise     =  solve' (targetSum-x) xs + solve' targetSum xs

{-
  Each path in the tree represents a subset of candidates.  Compared with using subsequences, it saves a lot of computation.

                                      solve (10,2)
                                    = solve' 10 [1,4,9]
                                          /       \
                                    10-1 /         \ 10
                                        /           \
                                    9 [4,9]        10 [4,9]
                                     /   \             /  \
                                9-4 /     \ 9    10-4 /    \ 10
                                   /       \         /      \
                               5 [9]     9 [9]    6 [9]     10 [9]
                              /\          /\         /\          /\
                         5-9 /  \ 5  9-9 /  \ 9  6-9/  \9  10-9 /  \ 10
                            /    \      /    \     /    \      /    \
                        -4 []  5 []  0 []  9 [] -3 [] 9 []   1 []   10 []
-}

{- slow due to subsequences
import Data.List (subsequences)

solve :: [String] -> String
solve [targetSumS, expS] =
  let targetSum :: Int
      targetSum = read targetSumS
      power :: Int
      power = read expS
      upperBound :: Int
      upperBound = (until (\n -> n^power > targetSum) (+1) 1) - 1
      candidates = map (^power) [1..upperBound]
  in
    (show . length . filter (\xs -> sum xs == targetSum) . subsequences) candidates
-}
