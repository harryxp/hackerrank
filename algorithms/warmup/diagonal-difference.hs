type Row = [Int]

main :: IO ()
main = (readLn :: IO Int)
  >>= \n -> getContents
  >>= print . diagonalDifference n . map (map read . words) . lines

diagonalDifference :: Int -> [Row] -> Int
diagonalDifference n = (\(_,s1,s2) -> abs(s1-s2)) . foldl (accumOneRow n) (0,0,0)

accumOneRow :: Int -> (Int,Int,Int) -> Row -> (Int,Int,Int)
accumOneRow n (rowCounter,s1,s2) row = (rowCounter+1, s1+(row!!rowCounter), s2+(row!!(n-1-rowCounter)))

