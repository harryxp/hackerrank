> import Control.Monad (foldM)
> import Text.ParserCombinators.ReadP ((+++),ReadP,char,readP_to_S)
> import Text.Printf (printf)

> import qualified Data.Map as Map (Map,fromList,lookup)

hackerrank doesn't support Literate Haskell files so you need to convert this file via:

  perl -ne 'if (m/^> (.*)$/) {print "$1\n";}' tree-of-life.lhs > tree-of-life.hs


Main
====

> main :: IO ()
> main = do
>   ruleNum <- readLn :: IO Int
>   treeStr <- getLine
>   let rule = buildRule ruleNum
>       tree = parseTree treeStr
>       trees = iterate (transformTree rule Top) tree -- infinite list
>   numQueries <- readLn :: IO Int
>   foldM (\tree query -> query tree) (tree,0) (replicate numQueries (runQuery trees rule))
>   return ()

> runQuery :: [Tree] -> Rule -> (Tree,Int) -> IO (Tree,Int)
> runQuery trees rule (tree,step) = do
>   stepAndPath <- getLine
>   let [newStep',path'] = words stepAndPath
>       newStep = (read newStep' + step)
>       path :: String
>       path = (tail . reverse . tail . reverse) path'
>       newTree :: Tree
>       newTree = trees !! newStep
>   putStrLn (followPath newTree path)
>   return (newTree,newStep)

> transformTree :: Rule -> Tree -> Tree -> Tree
> transformTree rule parent node@(Branch { left = l, right = r }) =
>   let key = map getCellValue [parent,l,node,r]
>       maybeValue = Map.lookup key rule
>   in case maybeValue of
>     Just v ->
>       Branch { value = v, left = transformTree rule node l, right = transformTree rule node r }
>     Nothing -> impossible
> transformTree rule parent node@(Leaf {}) =
>   let key = [getCellValue parent,'0',getCellValue node,'0']
>       maybeValue = Map.lookup key rule
>   in case maybeValue of
>     Just v -> Leaf { value = v }
>     Nothing -> impossible
> transformTree _ _ Top = impossible

> getCellValue :: Tree -> Char
> getCellValue Top = '0'
> getCellValue node = if value node then '1' else '0'

> followPath :: Tree -> String -> String
> followPath tree "" = if value tree then "X" else "."
> followPath (Branch { left = l }) ('<':xs) = followPath l xs
> followPath (Branch { right = r }) ('>':xs) = followPath r xs
> followPath _ _ = impossible


Tree
====

> data Tree = Top    | -- virtual node serves as the parent of root
>             Branch { value :: Bool
>                    , left :: Tree
>                    , right :: Tree
>                    }
>                    |
>             Leaf   { value :: Bool
>                    }

> instance Show Tree where
>   show (Branch { value = v, left = l, right = r }) =
>     printf "(%s %s %s)" (show l) (if v then "X" else ".") (show r)
>   show (Leaf { value = v }) = if v then "X" else "."
>   show Top = impossible

The string should be parsed into one tree, and one tree only.

> parseTree :: String -> Tree
> parseTree s = case readP_to_S treeP s of
>   [(tree,"")] -> tree
>   otherwise -> error "Can't parse the tree."
>
> treeP :: ReadP Tree
> treeP = onLeafP +++ offLeafP +++ branchP
>
> onLeafP :: ReadP Tree
> onLeafP = char 'X' >> return (Leaf { value = True })
>
> offLeafP :: ReadP Tree
> offLeafP = char '.' >> return (Leaf { value = False })
>
> branchP :: ReadP Tree
> branchP = do
>   char '('
>   lChild <- treeP
>   char ' '
>   v <- char 'X' +++ char '.'
>   char ' '
>   rChild <- treeP
>   char ')'
>   return (Branch { value = (v == 'X'), left = lChild, right = rChild })


Rule
====

The next state of a cell is determined by itself and 3 neighbors.  This means
that a rule (that describes all the behaviors of a given automaton) only needs
to specify a next state for each possible pattern of 4 cells:

1111 1110 1101 ... 0000

In this problem, this means 1 rule is just 16 pattern-state pairs, and there
are 2^16 possible rules.

> numCellsInNeighborhood = 4
> numStatesForEachCell = 2
> numPatternsForEachRule = numStatesForEachCell^numCellsInNeighborhood  -- 16

Build a rule that goes from cell patterns to True/False.  For example rule 7710 is
"0000" -> False
"0001" -> True
"0010" -> True
"0011" -> True
"0100" -> True
"0101" -> False
"0110" -> False
"0111" -> False
"1000" -> False
"1001" -> True
"1010" -> True
"1011" -> True
"1100" -> True
"1101" -> False
"1110" -> False
"1111" -> False

> type Rule = Map.Map String Bool
>
> buildRule :: Int -> Rule
> buildRule ruleNum =
>   let values :: [Bool]
>       values = map (=='1') (printf "%016b" ruleNum)
>       keys :: [String]
>       keys = map (printf "%04b") ([numPatternsForEachRule-1,numPatternsForEachRule-2..0]::[Int])
>   in Map.fromList (zip keys values)
>


Misc
====

> impossible :: a
> impossible = error "This cannot happen."

