import Data.List (intercalate, sort)

-- The top level functions are: main, rectree, showTrees and fractalize
--
main :: IO ()
main = getLine >>= \n -> (putStr . rectree) (read n)

data Tree = Tree { row::Int, column::Int, height::Int } deriving Show
-- row and column are the coordinate of the top vertex, and are 1-based
firstTree = Tree { row = 63, column = 50, height = 32 }

-- this basically calls `fractalize` n times and converts the result to a string
-- foldl returns [[Tree]], sorted by row in ascending order; each member [Tree] is sorted by column in ascending order
rectree :: Int -> String
rectree n = showTrees $
  foldl (\allTrees@(treesFromLastRound:_) _ -> (concat (map fractalize treesFromLastRound)):allTrees) [[firstTree]] [2..n]

-- converts groups of trees to the final output
-- assumes that [[Tree]] is sorted by row in ascending order; each member [Tree] is sorted by column in ascending order
showTrees :: [[Tree]] -> String
showTrees trees =
  let lines = (concat . map showLines) trees
      emptyLines = replicate (63-length lines) (replicate 100 '_' ++ "\n")
  in
    intercalate "" (emptyLines ++ lines)

-- takes a list of trees that are on the same rows (with the same heights)
-- converts them to a string
showLines :: [Tree] -> [String]
showLines [] = [""]                                           -- this should not happen
showLines trees@(Tree { row = _, column = c, height = h }:_) = map ((++ "\n") . showLine trees) [1..h]

showLine :: [Tree] -> Int -> String
showLine trees n = (\s -> s ++ replicate (100-length s) '_') $  -- complete the line by attaching the '_' tail
  snd $
  foldl (\(curStart, curString) col -> (col, curString ++ replicate (col-curStart-1) '_' ++ "1")) (0, "") $                                -- generates the main parts of the line: everything except the tail
  (sort . concat . map (\(Tree { row = _, column = c, height = h }) -> if n > halfH h then [c] else [c-halfH h+n-1, c+halfH h-n+1])) trees  -- turn trees into a list of numbers each of which indicates a "1" at that column
  where
    halfH h = div h 2

fractalize :: Tree -> [Tree]
fractalize Tree { row = r, column = c, height = h } =
  [ Tree { row = r - h, column = c - h `div` 2, height = h `div` 2 }
  , Tree { row = r - h, column = c + h `div` 2, height = h `div` 2 }
  ]

