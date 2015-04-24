import Data.List (groupBy, intercalate, sortBy)

-- The top level functions are: main, sierpinski, showTriangles and fractalize

main :: IO ()
main = getLine >>= \n -> (putStr . sierpinski) (read n)

data Triangle = Triangle { row::Int, column::Int, height::Int } deriving Show
-- row and column are the coordinate of the top vertex, and are 1-based
biggestTriangle = Triangle { row = 1, column = 32, height = 32 }

-- this basically calls `fractalize` n times and converts the result to a string
sierpinski :: Int -> String
sierpinski n = showTriangles $
  foldl (\triangles _ -> concat (map fractalize triangles)) [biggestTriangle] [1..n]

-- converts triangles to the final output
showTriangles :: [Triangle] -> String
showTriangles triangles = (intercalate "" . map showLines . groupByRow) triangles

groupByRow :: [Triangle] -> [[Triangle]]
groupByRow triangles =
  groupBy (\Triangle{row=r1,column=_,height=_} Triangle{row=r2,column=_,height=_} -> r1 == r2) $
  sortBy compareByColumn triangles

compareByColumn = \Triangle{row=r1,column=_,height=_} Triangle{row=r2,column=_,height=_} -> compare r1 r2

-- takes a list of triangles that are on the same rows (with the same heights)
-- converts them to a string
showLines :: [Triangle] -> String
showLines [] = ""                 -- this should not happen
showLines triangles@(Triangle { row = _, column = _, height = h }:_) = concat $ map ((++ "\n") . showLine) [1..h]
  where
    showLine :: Int -> String
    showLine n = (\s -> s ++ replicate (63-length s) '_') $  -- complete the line by attaching the '_' tail
      snd $
      foldl           -- generates the main parts of the line: everything except the tail
        (\(curStart, curString) (trianglePartStart, trianglePartLength) ->
          (trianglePartStart+trianglePartLength, curString ++ replicate (trianglePartStart-curStart) '_' ++ replicate trianglePartLength '1'))
        (0, "") $
      map (\Triangle { row = _, column = c, height = _ } -> (c-n, 2*(n-1)+1)) $   -- map each triangle to a tuple (start, length)
      sortBy compareByColumn triangles

fractalize :: Triangle -> [Triangle]
fractalize Triangle { row = r, column = c, height = h } =
  [ Triangle { row = r,             column = c,             height = h `div` 2 }
  , Triangle { row = r + h `div` 2, column = c - h `div` 2, height = h `div` 2 }
  , Triangle { row = r + h `div` 2, column = c + h `div` 2, height = h `div` 2 }
  ]

