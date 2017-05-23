> import Control.Applicative ((<|>))
> import Control.Monad (foldM)
> import Text.ParserCombinators.ReadP (ReadP, readP_to_S, satisfy)
> import Text.Printf (printf)

> import qualified Data.Map as Map (Map,fromList,lookup)


Main
====

> main :: IO Tree
> main = do
>   ruleNum <- readLn :: IO Int
>   treeStr <- getLine
>   let rule = buildRule ruleNum
>       tree = parseTree treeStr
>   numQueries <- readLn :: IO Int
>   foldM (\tree query -> query tree) tree (replicate numQueries (runQuery rule))

> runQuery :: Rule -> Tree -> IO Tree
> runQuery rule tree = do
>   numStepsAndPath <- getLine
>   let [numSteps',path'] = words numStepsAndPath
>       numSteps :: Int
>       numSteps = read numSteps'
>       path :: String
>       path = (tail . reverse . tail . reverse) path'
>       newTree :: Tree
>       newTree = transformTree rule tree Top
>   putStrLn (followPath newTree path)
>   return newTree

> transformTree :: Rule -> Tree -> Tree -> Tree
> transformTree rule node@(Branch { left = l, right = r }) parent =
>   let key = map getCellValue [parent,l,node,r]
>       maybeValue = Map.lookup key rule
>   in case maybeValue of
>     Just v ->
>       Branch { value = v, left = transformTree rule l node, right = transformTree rule r node }
>     Nothing -> impossible
> transformTree rule node@(Leaf {}) parent =
>   let key = [getCellValue parent,'0',getCellValue node,'0']
>       maybeValue = Map.lookup key rule
>   in case maybeValue of
>     Just v -> Leaf { value = v }
>     Nothing -> impossible
> transformTree _ Top _ = impossible

> getCellValue :: Tree -> Char
> getCellValue (Branch { value = v }) = if v then '1' else '0'
> getCellValue (Leaf { value = v })  = if v then '1' else '0'
> getCellValue Top = '0'

> followPath :: Tree -> String -> String
> followPath = undefined

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
> treeP = onLeafP <|> offLeafP <|> branchP
>
> onLeafP :: ReadP Tree
> onLeafP = satisfy (== 'X') >> return (Leaf { value = True })
>
> offLeafP :: ReadP Tree
> offLeafP = satisfy (== '.') >> return (Leaf { value = False })
>
> branchP :: ReadP Tree
> branchP = do
>   satisfy (== '(')
>   lChild <- treeP
>   satisfy (== ' ')
>   v <- satisfy (== 'X') <|> satisfy (== '.')
>   satisfy (== ' ')
>   rChild <- treeP
>   satisfy (== ')')
>   return (Branch { value = (v == 'X'), left = lChild, right = rChild })


Rule
====

The next state of a cell is determined by itself and 3 neighbors.  This means
that a rule (that describes all the behaviors of a given automaton) only needs
to specify a next state for each possible pattern of 4 cells:

1111 1110 1101 ... 0000

In this problem, this means 1 rules is just 16 pattern-state pairs, and there
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
>       values = map (\c -> if c=='1' then True else False) (printf "%016b" ruleNum)
>       keys :: [String]
>       keys = map (printf "%04b") ([numPatternsForEachRule-1,numPatternsForEachRule-2..0]::[Int])
>   in Map.fromList (zip keys values)
>


Misc
====

> impossible :: a
> impossible = error "This cannot happen."

