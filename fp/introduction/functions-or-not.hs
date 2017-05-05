import Data.List (findIndex,sort)
import qualified Data.Map.Lazy as Map

isFunction :: [(Int,Int)] -> Bool
isFunction xs = isFunction' Map.empty xs

isFunction' m [] = True
isFunction' m ((k,v):xs) = case Map.lookup k m of
  Nothing -> isFunction' (Map.insert k v m) xs
  Just v' | v == v' -> isFunction' m xs
          | otherwise -> False

main :: IO [()]
main = getContents >>= mapM putStrLn . reverse . map (\b -> if b then "YES" else "NO") . runTestCases . lines

runTestCases :: [String] -> [Bool]
runTestCases (line:lines) =
  let testCases :: [[(Int,Int)]]
      testCases = foldl parseLine [] lines
  in
    map isFunction testCases
runTestCases _ = error "This cannot happen."

parseLine :: [[(Int,Int)]] -> String -> [[(Int,Int)]]
parseLine (x:xs) s = case findIndex (==' ') s of
  Nothing -> []:x:xs
  Just n -> let (h,t) = splitAt n s in ((read h,read t):x):xs
parseLine [] _ = [[]]

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
