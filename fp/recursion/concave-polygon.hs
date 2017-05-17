import Data.List ((\\),maximumBy,partition)

type Point = (Int,Int)
type Line = (Point,Point)

solve :: [Point] -> Bool
solve pts = let convextHull = quickHull pts in
  case pts \\ convextHull of
    [] -> False
    xs -> if pointsOnConvexHull convextHull xs then False else True

pointsOnConvexHull :: [Point] -> [Point] -> Bool
pointsOnConvexHull convextHull pts = and $ map (pointOnConvexHull convextHull) pts

pointOnConvexHull :: [Point] -> Point -> Bool
pointOnConvexHull convextHull@(x:xs) pt = let lines = zip convextHull (xs ++ [x]) in
  or $ map (\line -> isOn line pt) lines

isOn :: Line -> Point -> Bool
isOn line = not . isNotOn line

main :: IO ()
main = (readLn :: IO Int)
  >> getContents
  >>= putStrLn . (\b -> if b then "YES" else "NO") . solve . map ((\[x,y] -> (read x,read y)) . words) . lines


-- copied from convex-hull.lhs

quickHull :: [Point] -> [Point]
quickHull points =
  let
    leftmost = minimum points   -- lower left when there's a tie
    rightmost = maximum points  -- upper right when there's a tie
    line :: Line
    line = (leftmost,rightmost)
    (leftHalf,rightHalf) =  -- the line divides the convex hull
      partition (isToTheLeftOf line) $ filter (isNotOn line) points
  in
    (leftmost:findHull leftHalf line True) ++
    (rightmost:findHull rightHalf line False)
isToTheLeftOf :: Line -> Point -> Bool
isToTheLeftOf line pt = linePointPosition line pt > 0
isToTheRightOf :: Line -> Point -> Bool
isToTheRightOf line pt = linePointPosition line pt < 0
isNotOn :: Line -> Point -> Bool
isNotOn line pt = linePointPosition line pt /= 0
linePointPosition :: Line -> Point -> Int
linePointPosition ((x1,y1),(x2,y2)) (x,y) = (x2-x1) * (y-y1) - (x-x1) * (y2-y1)
findHull :: [Point] -> Line -> Bool -> [Point]
findHull [] _ _ = []
findHull pts line@(ptLeft,ptRight) shouldSearchLeftward =
  let
    farthest = maximumBy (compareDistances line) pts
    newLine1 = (ptLeft,farthest)
    newLine2 = (farthest,ptRight)
    pred = if shouldSearchLeftward then isToTheLeftOf else isToTheRightOf
    newPts1 = filter (pred newLine1) pts
    newPts2 = filter (pred newLine2) pts
  in
    if shouldSearchLeftward
    then findHull newPts1 newLine1 shouldSearchLeftward ++ (farthest:findHull newPts2 newLine2 shouldSearchLeftward)
    else findHull newPts2 newLine2 shouldSearchLeftward ++ (farthest:findHull newPts1 newLine1 shouldSearchLeftward)
compareDistances :: Line -> Point -> Point -> Ordering
compareDistances line p1 p2 =
  compare (distanceToLine line p1) (distanceToLine line p2)
distanceToLine :: Line -> Point -> Double
distanceToLine ((x1,y1),(x2,y2)) (x,y) =
  fromIntegral (abs ((x2 - x1) * (y1 - y) - (x1 - x) * (y2 - y1))) / sqrt (fromIntegral ((x2 - x1) ^ 2 + (y2 - y1) ^ 2))
