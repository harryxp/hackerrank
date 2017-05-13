import Data.List (maximumBy,partition,sortOn)
import Text.Printf

-- Quickhull implementation

-- TODO could use a faster thing than maximumBy

type Point = (Int,Int)
type Line = (Point,Point)

solve :: [Point] -> Double
solve = perimeter . arrangePointsClockwise . findConvexHull

findConvexHull :: [Point] -> [Point]
findConvexHull points =
  let
    leftmost = minimum points
    rightmost = maximum points
    -- leftmost and rightmost define a line
    line = (leftmost,rightmost)
    -- the line divides the convex hull
    (leftHalf,rightHalf) =
      partition (isOnLeftOf line) $ filter (isNotOn line) points
 in
   [leftmost,rightmost] ++
   findConvexHull' leftHalf line isOnLeftOf ++
   findConvexHull' rightHalf line isOnRightOf

isOnLeftOf :: Line -> Point -> Bool
isOnLeftOf line pt = linePointPosition line pt > 0

isOnRightOf :: Line -> Point -> Bool
isOnRightOf line pt = linePointPosition line pt < 0

isNotOn :: Line -> Point -> Bool
isNotOn line pt = linePointPosition line pt /= 0

-- http://stackoverflow.com/questions/1560492/how-to-tell-whether-a-point-is-to-the-right-or-left-side-of-a-line
-- decide the point's position relative to the line
linePointPosition :: Line -> Point -> Int
linePointPosition ((x1,y1),(x2,y2)) (x,y) = (x2 - x1) * (y - y1) - (x - x1) * (y2 - y1)

findConvexHull' :: [Point] -> Line -> (Line -> Point -> Bool) -> [Point]
findConvexHull' [] _ _ = []
findConvexHull' pts line@(ptLeft,ptRight) pred =
  let
    farthest = maximumBy (compareDistances line) pts
    newLine1 = (ptLeft,farthest)
    newLine2 = (farthest,ptRight)
    newPts1 = filter (pred newLine1) pts
    newPts2 = filter (pred newLine2) pts
  in
    [farthest] ++
    findConvexHull' newPts1 newLine1 pred ++
    findConvexHull' newPts2 newLine2 pred

-- compare the distances of p1 and p2 to a line
compareDistances :: Line -> Point -> Point -> Ordering
compareDistances line p1 p2 =
  compare (distanceToLine line p1) (distanceToLine line p2)

-- http://mathworld.wolfram.com/Point-LineDistance2-Dimensional.html
distanceToLine :: Line -> Point -> Double
distanceToLine ((x1,y1),(x2,y2)) (x,y) =
  fromIntegral (abs ((x2 - x1) * (y1 - y) - (x1 - x) * (y2 - y1))) / sqrt (fromIntegral ((x2 - x1) ^ 2 + (y2 - y1) ^ 2))

-- copied from ../introduction/polygon-perimeter.hs
perimeter :: [Point] -> Double
perimeter (x:xs) = snd $
  foldl (\(prevP,perim) p -> (p,perim + distance prevP p)) (x,0) $ xs ++ [x]
  where
    distance :: Point -> Point -> Double
    distance p1 p2 = sqrt . fromIntegral $
      (fst p2 - fst p1) ^ 2 + (snd p2 - snd p1) ^ 2
perimeter [] = error "This cannot happen."

arrangePointsClockwise :: [Point] -> [Point]
arrangePointsClockwise pts =
  let numPoints = (fromIntegral . length) pts
      (xm,ym) = (((/numPoints) . fromIntegral . sum . map fst) pts, ((/numPoints) . fromIntegral . sum . map snd) pts)
  in sortOn (\(x,y) -> fromIntegral (y-ym) / fromIntegral (x-xm))



main :: IO ()
main = do
  n <- readLn :: IO Int
  content <- getContents
  let
    points = map (\[x, y] -> (x, y)). map (map (read::String->Int)). map words. lines $ content
    ans = solve points
  printf "%.1f\n" ans

