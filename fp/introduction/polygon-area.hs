import Text.Printf (printf)

type Point = (Double,Double)

main :: IO ()
main = getContents >>= putStrLn . printf "%.1f" . area . parseLines . lines

parseLines :: [String] -> [Point]
parseLines (_:xs) = map ((\(y1:y2:_) -> (y1,y2)) . map read . words) xs
parseLines [] = error "This cannot happen."

area :: [Point] -> Double
area pts@(x:xs) =
  let shiftedPts = xs ++ [x]
      shiftedPairs :: [((Double,Double),(Double,Double))]
      shiftedPairs = zip pts shiftedPts
  in
    (positives shiftedPairs + negatives shiftedPairs) / 2
  where
    positives :: [((Double,Double),(Double,Double))] -> Double
    positives = foldl (\s ((x1,_),(_,y2)) -> s + x1 * y2) 0
    negatives :: [((Double,Double),(Double,Double))] -> Double
    negatives = foldl (\s ((_,y1),(x2,_)) -> s - y1 * x2) 0
area [] = error "This cannot happen."
