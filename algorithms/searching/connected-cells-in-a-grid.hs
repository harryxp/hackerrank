import qualified Data.Set as S

import Debug.Trace

type Matrix = [[Int]]
type Row = [Int]
type Coord = (Int,Int)

main :: IO ()
main = getLine >> getLine >> getContents >>= print . solve . map (map read . words) . lines

solve :: Matrix -> Int
solve m =
  let
    ones :: S.Set Coord
    ones = foldl accumRow S.empty (zip [0..] m)
  in (maximum . map S.size . regions) ones

accumRow :: S.Set Coord -> (Int,Row) -> S.Set Coord
accumRow ones (rowIdx,row) = foldl accumCell ones (map (\(colIdx,cell) -> (rowIdx,colIdx,cell)) (zip [0..] row))

accumCell :: S.Set Coord -> (Int,Int,Int) -> S.Set Coord
accumCell cs (rowIdx,colIdx,cell) = case cell == 1 of
  False -> cs
  True -> S.insert (rowIdx,colIdx) cs

regions :: S.Set Coord -> [S.Set Coord]
regions = S.foldl mergeCoord []

mergeCoord :: [S.Set Coord] -> Coord -> [S.Set Coord]
mergeCoord ss coord = case connect coord ss of
  (connected,rest) -> S.unions (S.singleton coord:connected):rest

connect :: Coord -> [S.Set Coord] -> ([S.Set Coord],[S.Set Coord])
connect coord = foldl (\(connected,rest) cs -> if isAdjacent coord cs then (cs:connected,rest) else (connected,cs:rest)) ([],[])

isAdjacent :: Coord -> S.Set Coord -> Bool
isAdjacent (x,y) = S.foldl (||) False . S.map (\(w,v) -> let dx = abs(x-w); dy = abs(y-v) in
  dx == 0 && dy == 1 ||
  dx == 1 && dy == 0 ||
  dx == 1 && dy == 1)

