type Point = (Float,Float)

main :: IO ()
main = getContents >>= putStrLn . show . perimeter . parseLines . lines

parseLines :: [String] -> [Point]
parseLines (_:xs) = map ((\(y1:y2:ys) -> (y1,y2)) . map read . words) xs
parseLines [] = error "This cannot happen."

perimeter :: [Point] -> Float
perimeter (x:xs) = snd $ foldl (\(prevP,perim) p -> (p,perim + distance prevP p)) (x,0) $ xs ++ [x]
perimeter [] = error "This cannot happen."

distance :: Point -> Point -> Float
distance p1 p2 = sqrt $ (fst p2 - fst p1) ** 2 + (snd p2 - snd p1) ** 2
