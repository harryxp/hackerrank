> import Data.List (maximumBy,partition,sortOn)
> import Text.Printf

This is an implementation of the Quickhull algorithm.  See https://en.wikipedia.org/wiki/Quickhull.

TODO could use a faster thing than maximumBy

> type Point = (Int,Int)
> type Line = (Point,Point)

> solve :: [Point] -> Double
> solve = perimeter . quickHull

Entrance of the Quickhull algorithm.  Basically prepare the data for recursion in findHull.

Note the way we arrange leftmost and rightmost points - it's crucial to do this so that the points are arranged clockwise.

> quickHull :: [Point] -> [Point]
> quickHull points =
>   let
>     leftmost = minimum points
>     rightmost = maximum points
>     line :: Line
>     line = (leftmost,rightmost)
>     (leftHalf,rightHalf) =  -- the line divides the convex hull
>       partition (isOnLeftOf line) $ filter (isNotOn line) points
>   in
>     (leftmost:findHull leftHalf line True) ++
>     (rightmost:findHull rightHalf line False)

Decide the point's position relative to the line.
http://stackoverflow.com/questions/1560492/how-to-tell-whether-a-point-is-to-the-right-or-left-side-of-a-line

> isOnLeftOf :: Line -> Point -> Bool
> isOnLeftOf line pt = linePointPosition line pt > 0

> isOnRightOf :: Line -> Point -> Bool
> isOnRightOf line pt = linePointPosition line pt < 0

> isNotOn :: Line -> Point -> Bool
> isNotOn line pt = linePointPosition line pt /= 0

> linePointPosition :: Line -> Point -> Int
> linePointPosition ((x1,y1),(x2,y2)) (x,y) = (x2-x1) * (y-y1) - (x-x1) * (y2-y1)

Recursively find the hull.  shouldSearchLeftward is used to guide:
1. Which direction should we look for the farthest point, and
2. How the points should be arranged to be clockwise.

> findHull :: [Point] -> Line -> Bool -> [Point]
> findHull [] _ _ = []
> findHull pts line@(ptLeft,ptRight) shouldSearchLeftward =
>   let
>     farthest = maximumBy (compareDistances line) pts
>     newLine1 = (ptLeft,farthest)
>     newLine2 = (farthest,ptRight)
>     pred = if shouldSearchLeftward then isOnLeftOf else isOnRightOf
>     newPts1 = filter (pred newLine1) pts
>     newPts2 = filter (pred newLine2) pts
>   in
>     if shouldSearchLeftward
>     then findHull newPts1 newLine1 shouldSearchLeftward ++ (farthest:findHull newPts2 newLine2 shouldSearchLeftward)
>     else findHull newPts2 newLine2 shouldSearchLeftward ++ (farthest:findHull newPts1 newLine1 shouldSearchLeftward)

Compare the distances of p1 and p2 to a line.

> compareDistances :: Line -> Point -> Point -> Ordering
> compareDistances line p1 p2 =
>   compare (distanceToLine line p1) (distanceToLine line p2)

http://mathworld.wolfram.com/Point-LineDistance2-Dimensional.html

> distanceToLine :: Line -> Point -> Double
> distanceToLine ((x1,y1),(x2,y2)) (x,y) =
>   fromIntegral (abs ((x2 - x1) * (y1 - y) - (x1 - x) * (y2 - y1))) / sqrt (fromIntegral ((x2 - x1) ^ 2 + (y2 - y1) ^ 2))

Copied from ../introduction/polygon-perimeter.hs.  It assumes that the points are ordered clockwise or counterclockwise.

> perimeter :: [Point] -> Double
> perimeter (x:xs) = snd $
>   foldl (\(prevP,perim) p -> (p,perim + distance prevP p)) (x,0) $ xs ++ [x]
>   where
>     distance :: Point -> Point -> Double
>     distance p1 p2 = sqrt . fromIntegral $
>       (fst p2 - fst p1) ^ 2 + (snd p2 - snd p1) ^ 2
> perimeter [] = error "This cannot happen."


> main :: IO ()
> main = do
>   n <- readLn :: IO Int
>   content <- getContents
>   let
>     points = map (\[x, y] -> (x, y)). map (map (read::String->Int)). map words. lines $ content
>     ans = solve points
>   printf "%.1f\n" ans

