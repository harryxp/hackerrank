import Text.Printf (printf)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double] --Complete this function--
solve l r a b = let pair = foldl sumPairs (0, 0) (map solveTerm (zip a b)) in [fst pair, snd pair]
  where
    sumPairs = \(a1, v1) (a2, v2) -> (a1+a2, v1+v2)
    solveTerm :: (Int, Int) -> (Double, Double)
    solveTerm (c, e) =
      (calcArea c e (fromIntegral l) 0, calcVolume c e (fromIntegral l) 0)
    dx = 0.001

    calcArea :: Int -> Int -> Double -> Double -> Double
    calcArea c e x areaAccum =
      if x > (fromIntegral r)
      then areaAccum
      else calcArea c e (x+dx) (areaAccum + dx * f c e x)

    calcVolume :: Int -> Int -> Double -> Double -> Double
    calcVolume c e x volAccum =
      if x > (fromIntegral r)
      then volAccum
      else calcVolume c e (x+dx) (volAccum + dx * pi * (f c e x)^2)
    f :: Int -> Int -> Double -> Double
    f c e x = (fromIntegral c) * (x^e)

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines

