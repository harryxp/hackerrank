import Control.Monad (replicateM)
import Text.Printf

import Test.HUnit

type Point = (Int,Int)

main :: IO ()
main = readLn >>= \n -> replicateM n getLine >>= \lns ->
  let
    ws :: [[String]]    -- [["1","1"], ["2","2"]]
    ws =  map words lns
    points :: [Point]
    points =  [ (x,y) | [x,y] <- map (map (read::String->Int)) ws ]
    ans = solve points
  in printf "%.1f\n" ans

solve        :: [Point] -> Double
solve []     =  0
solve points =
  let (leftMostPoint, rightMostPoint) = findLeftAndRightMostPoints points
  in
    findMostRemotePointFrom leftMostPoint rightMostPoint

{-
solve                  :: [Point] -> Double
solve []               =  0
solve (p@(x,y):points) =
  let (minX,maxX,minY,maxY,minXPoints,maxXPoints,minYPoints,maxYPoints) = foldl iteratePoint (x,x,y,y,[p],[p],[p],[p]) points
  in undefined

iteratePoint :: (Int,Int,Int,Int,[Point],[Point],[Point],[Point]) -> Point -> (Int,Int,Int,Int,[Point],[Point],[Point],[Point])
iteratePoint (minX,maxX,minY,maxY,minXPoints,maxXPoints,minYPoints,maxYPoints) p@(x,y) =
  let (newMinX,newMinXPoints) = iteratePoint' p x (<) minX minXPoints
      (newMaxX,newMaxXPoints) = iteratePoint' p x (>) maxX maxXPoints
      (newMinY,newMinYPoints) = iteratePoint' p y (<) minY minYPoints
      (newMaxY,newMaxYPoints) = iteratePoint' p y (>) maxY maxYPoints
  in (newMinX,newMaxX,newMinY,newMaxY,newMinXPoints,newMaxXPoints,newMinYPoints,newMaxYPoints)

iteratePoint' :: Point -> Int -> (Int -> Int -> Bool) -> Int -> [Point] -> (Int,[Point])
iteratePoint' p a cmp minOrMax oldSet
  | a == minOrMax  = (a,p:oldSet)
  | cmp a minOrMax = (a,[p])
  | otherwise      = (minOrMax,oldSet)

--------

tests = TestList
  [ TestLabel "iteratePoint" testIteratePoint ]

testIteratePoint = TestCase (assertEqual "iteratePoint" expected actual)
  where
    expected = (1,5,1,5,[(1,1)],[(5,3)],[(1,1)],[(2,5)])
    actual = iteratePoint (2,5,2,5,[(2,2),(2,5)],[(5,3)],[(3,2),(2,2)],[(2,5)]) (1,1)

-}
