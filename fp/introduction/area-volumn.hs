import Text.Printf (printf)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double] --Complete this function--
solve l r a b = sumSliceAreaAndVolume $ map
    (calcSliceAreaAndVolume (zip a b))
    [fromIntegral l, (fromIntegral l)+dx..fromIntegral r]
  where
    dx = 0.001
    sumSliceAreaAndVolume :: [(Double, Double)] -> [Double]
    sumSliceAreaAndVolume lst = let areaAndVolume = foldl (\(a1, v1) (a2, v2) -> (a1+a2, v1+v2)) (0, 0) lst in
      [fst areaAndVolume, snd areaAndVolume]
    -- calcSliceAreaAndVolume takes all the coefficients, all the exponents, x,
    -- and spits out area and volume for the tiny little slice at x
    calcSliceAreaAndVolume :: [(Int, Int)] -> Double -> (Double, Double)
    calcSliceAreaAndVolume pairs x = let
      y = foldl (\accum (c, e) -> accum + (fromIntegral c) * x**(fromIntegral e)) 0 pairs
      sliceArea = dx * y
      sliceVolume = dx * pi * y^2
      in
      (sliceArea, sliceVolume)

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines

-- test case #3
-- input
-- -1 2 0 2 -1 -3 -4 -1 -3 -4 -999 1 2 3 4 5
-- -15 -14 -13 -12 -11 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0
-- 1 10
-- output
-- -193.1
-- 336642.8

