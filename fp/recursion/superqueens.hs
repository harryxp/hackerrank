main :: IO ()
main =  getLine >>= putStrLn . show . length . superQueens . read

type CandidateSolution = [(Int,Int)]

superQueens   :: Int -> [CandidateSolution]
superQueens n =  solve n n

solve               :: Int -> Int -> [CandidateSolution]
solve boardSize 1   =  map (\col -> [(1,col)]) [1..boardSize]
solve boardSize row =
  let candidateSolutions = solve boardSize (row-1)
  in concat (map (addOneRow boardSize row) candidateSolutions)

addOneRow                                 :: Int -> Int -> CandidateSolution -> [CandidateSolution]
addOneRow boardSize row candidateSolution =
  let queenPositions = map (\col -> (row,col)) [1..boardSize]
      validQueenPositions = filter (haveNoConflict candidateSolution) queenPositions
  in map (:candidateSolution) validQueenPositions

haveNoConflict          :: CandidateSolution -> (Int,Int) -> Bool
haveNoConflict []     _ =  True
haveNoConflict (x:xs) p =  haveNoConflict' x p && haveNoConflict xs p

haveNoConflict'                 :: (Int,Int) -> (Int,Int) -> Bool
haveNoConflict' (x1,y1) (x2,y2) =  not (                                    -- not
  x1 == x2 ||                                                               -- in the same row, or
  y1 == y2 ||                                                               -- in the same column, or
  abs(x1-x2) == abs(y1 - y2) ||                                             -- in the same diagonal line, or
  abs(x1-x2) == 2 && abs(y1-y2) == 1 || abs(x1-x2) == 1 && abs(y1-y2) == 2) -- in a L-shape

