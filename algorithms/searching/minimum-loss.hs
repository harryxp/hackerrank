import Data.List (find,sortOn)

type DayPrice = (Int,Int)

main :: IO ()
main = getLine >>
       getLine >>=
       print . searchMinimumLoss . sortByDiffAsc . buildDayPricePairs . sortByPriceDesc . zip [1..] . map read . words

sortByPriceDesc :: [DayPrice] -> [DayPrice]
sortByPriceDesc = sortOn (\(_,price) -> (-price))

buildDayPricePairs :: [DayPrice] -> [(DayPrice,DayPrice)]
buildDayPricePairs [x,y] = [(x,y)]
buildDayPricePairs (x:y:xs) = (x,y):buildDayPricePairs (y:xs)

sortByDiffAsc :: [(DayPrice,DayPrice)] -> [(DayPrice,DayPrice)]
sortByDiffAsc = sortOn (\((_,p1),(_,p2)) -> p1-p2)

searchMinimumLoss :: [(DayPrice,DayPrice)] -> Int
searchMinimumLoss pairs = case find (\((d1,_),(d2,_)) -> d1 < d2) pairs of
  Just ((_,p1),(_,p2)) -> p1-p2
  Nothing -> error "This cannot happen."

