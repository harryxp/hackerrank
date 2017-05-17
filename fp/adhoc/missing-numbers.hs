import Data.List (intercalate,sort)
import qualified Data.Map as Map

main :: IO ()
main = getContents >>= putStrLn . intercalate " " . map show . solve . getAAndB . lines

getAAndB :: [String] -> ([Int],[Int])
getAAndB [_,a,_,b] = (map read (words a),map read (words b))

solve :: ([Int],[Int]) -> [Int]
solve (xs,ys) =
  let mx = buildFrequencyMap xs
      my = buildFrequencyMap ys
  in sort $ Map.foldlWithKey (accumMissingElement mx) [] my

buildFrequencyMap :: [Int] -> Map.Map Int Int
buildFrequencyMap = foldl (\m n -> Map.insertWith (\nv ov -> ov + nv) n 1 m) Map.empty

accumMissingElement :: Map.Map Int Int -> [Int] -> Int -> Int -> [Int]
accumMissingElement mx acc key val = case Map.lookup key mx of
  Nothing -> key:acc
  Just v | val > v -> key:acc
         | val == v -> acc
         | val < v -> error "This cannot happen."
