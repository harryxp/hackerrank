import Control.Monad (replicateM)

data BinaryTree = Branch { idx    :: Int
                         , depth  :: Int
                         , left   :: BinaryTree
                         , right  :: BinaryTree
                         }
                | Empty  { depth  :: Int
                         }
  deriving Show

main :: IO ()
main = do
  numNodes <- readLn :: IO Int
  nodeLines <- replicateM numNodes getLine
  numKs <- readLn :: IO Int
  ks <- replicateM numKs readLn :: IO [Int]
  let tree = (buildTree . map (map read . words)) nodeLines
  swapKs ks tree

buildTree :: [[Int]] -> BinaryTree
buildTree [] = error "impossbile"
buildTree subtrees = buildNode 1 1 subtrees

buildNode :: Int -> Int -> [[Int]] -> BinaryTree
buildNode (-1) d _ = Empty d
buildNode i d subtrees =
  case subtrees !! (i-1) of
    [left,right] -> Branch i d (buildNode left (d+1) subtrees) (buildNode right (d+1) subtrees)

swapKs :: [Int] -> BinaryTree -> IO ()
swapKs [] t = return ()
swapKs (k:ks) t = let newTree = (swapK k t) in inOrder newTree >> swapKs ks newTree

swapK :: Int -> BinaryTree -> BinaryTree
swapK _ e@(Empty _) = e
swapK k b@(Branch n d l r) = case d `mod` k of
  0 -> Branch n d (swapK k r) (swapK k l)
  otherwise -> Branch n d (swapK k l) (swapK k r)

inOrder :: BinaryTree -> IO ()
inOrder t = inOrder' t >> putStr "\n"
  where
    inOrder' (Empty _) = return ()
    inOrder' (Branch n _ l r) = inOrder' l >> putStr (show n ++ " ") >> inOrder' r

