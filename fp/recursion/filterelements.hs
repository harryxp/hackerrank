import Control.Monad (replicateM)
import Data.List
import qualified Data.Map.Strict as Map

import Test.HUnit

main :: IO ()
main =  handleInput >>= handleOutput . (map solve)

tests = TestList
  [ TestLabel "solve" testSolve
  , TestLabel "handleOneElement" testHandleOneElement
  ]

----

handleInput :: IO [(Int,[Int])]
handleInput  =
  getLine >>=
  \n -> replicateM (read n * 2) getLine >>=
  \lines -> return (makePairs lines)

makePairs          :: [String] -> [(Int,[Int])]
makePairs []       =  []
makePairs (x:y:xs) =
  let [n,k] =  (map read . words) x
      numbers = (map read . words) y
  in (k,numbers) : makePairs xs

----

solve        :: (Int,[Int]) -> [Int]
solve (k,xs) =
  let (_,dict) = foldl handleOneElement (0,Map.empty) xs
      candidateDict :: Map.Map Int (Int,Int,Int)
      candidateDict =  Map.filter (\(_,_,numOfAppearances) -> numOfAppearances >= k) dict
  in
    -- if you have the lastest Data.List, use sortOn instead:
    -- (map (\(i,_,_) -> i) . sortOn (\(_,pos,_) -> pos) . Map.elems) candidateDict
    (map (\(i,_,_) -> i) . sortBy (\(_,pos1,_) (_,pos2,_) -> compare pos1 pos2) . Map.elems) candidateDict

testSolve = TestCase (assertEqual "solve" expected (solve input))
  where
    input = (2,[4,5,2,5,4,3,1,3,4])
    expected = [4,5,3]

handleOneElement                :: (Int,Map.Map Int (Int,Int,Int)) -> Int -> (Int,Map.Map Int (Int,Int,Int))
handleOneElement (index,dict) i =  let maybeTuple = Map.lookup i dict in case maybeTuple of
  Just (i,pos,numOfAppearances) -> (index+1,Map.insert i (i,pos,numOfAppearances+1) dict)
  Nothing                       -> (index+1,Map.insert i (i,index,1) dict)

testHandleOneElement = TestCase (assertEqual "handleOneElement" expected (handleOneElement (index,dict) i))
  where
    index = 10
    dict = Map.fromList [(7,(7,4,2)),(2,(2,3,1))]
    i = 7
    expected =  (11,Map.fromList [(7,(7,4,3)),(2,(2,3,1))])

----

handleOutput          :: [[Int]] -> IO ()
handleOutput []       =  return ()
handleOutput ([]:xss) =  putStrLn "-1" >> handleOutput xss
handleOutput (xs:xss) =  putStrLn (intercalate " " (map show xs)) >> handleOutput xss

