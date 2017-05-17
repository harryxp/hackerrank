main :: IO ()
main = do
  ruleNum <- readLn :: IO Int
  treeStr <- getLine
  let tree = parseTree treeStr
  undefined

data Tree = Branch { value :: Bool
                   , left :: Tree
                   , right :: Tree
                   , parent :: Tree
                   } |
            Leaf   { value :: Bool
                   , parent :: Tree
                   } |
            Nil     -- parent of root, also serves as a placeholder

parseTree :: String -> Tree
parseTree = undefined
