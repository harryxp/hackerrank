import Data.List (findIndex)
import qualified Data.Map.Lazy as Map

main :: IO [()]
main = getContents >>= mapM putStrLn . map (\b -> if b then "YES" else "NO") . runTestCases . lines

runTestCases :: [String] -> [Bool]
runTestCases (_:lines) =
  let testCases :: [[(Int,Int)]]
      testCases = (reverse . foldl parseLine []) lines
  in
    map isFunction testCases
runTestCases [] = error "This cannot happen."

parseLine :: [[(Int,Int)]] -> String -> [[(Int,Int)]]
parseLine (x:xs) s = case findIndex (==' ') s of
  Nothing -> []:x:xs
  Just n -> let (h,t) = splitAt n s in ((read h,read t):x):xs
parseLine [] _ = [[]]

isFunction :: [(Int,Int)] -> Bool
isFunction xs = all snd $ scanl
  (
    \(m,isLegal) (k,v) -> case Map.lookup k m of
      Nothing -> ((Map.insert k v m),True)
      Just v' | v == v' -> (m,True)
              | otherwise -> (m,False)
  )
  (Map.empty,True)
  xs

{--
Sample Input:
2
5
1 2
2 3
3 4
5 5
4 5
3
1 1
2 1
2 2

Sample Output:
YES
NO
 --}
