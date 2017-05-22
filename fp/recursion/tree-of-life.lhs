> import Control.Applicative ((<|>))
> import Text.ParserCombinators.ReadP (ReadP, readP_to_S, satisfy)
> import Text.Printf (printf)

> main :: IO ()
> main = do
>   ruleNum <- readLn :: IO Int
>   treeStr <- getLine
>   let tree = parseTree treeStr
>   undefined

> data Tree = Branch { value :: Bool
>                    , left :: Tree
>                    , right :: Tree
>                    , parent :: Tree
>                    } |
>             Leaf   { value :: Bool
>                    , parent :: Tree
>                    } |
>             Nil     -- parent of root, also serves as a placeholder

The `show` function ignores parents to avoid indefinite loops.

> instance Show Tree where
>   show (Branch { value = v, left = l, right = r, parent = _ }) =
>     printf "Branch { value = %s, left = %s, right %s }" (show v) (show l) (show r)
>   show (Leaf { value = v, parent = _ }) = printf "Leaf { value = %s }" (show v)
>   show Nil = "Nil"

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
> onLeafP = satisfy (== 'X') >> return (Leaf { value = True, parent = Nil })
> 
> offLeafP :: ReadP Tree
> offLeafP = satisfy (== '.') >> return (Leaf { value = False, parent = Nil })
> 
> branchP :: ReadP Tree
> branchP = do
>   satisfy (== '(')
>   lChild <- treeP
>   satisfy (== ' ')
>   value <- satisfy (== 'X') <|> satisfy (== '.')
>   satisfy (== ' ')
>   rChild <- treeP
>   satisfy (== ')')
>   return (buildBranch value lChild rChild)
> 
> buildBranch :: Char -> Tree -> Tree -> Tree
> buildBranch v lChild rChild =
>   let b = if v == 'X' then True else False
>       br = Branch { value = b, left = lChild', right = rChild', parent = Nil }
>       lChild' = setParent br lChild
>       rChild' = setParent br rChild
>   in br
>   where
>     setParent _ Nil = Nil
>     setParent br (Leaf { value = v, parent = Nil }) = Leaf { value = v, parent = br }
>     setParent br (Branch { value = v, left = l, right = r, parent = Nil }) =
>       Branch { value = v, left = l, right = r, parent = br }
>     setParent _ t = error ("This is an impossible case" ++ show t)

