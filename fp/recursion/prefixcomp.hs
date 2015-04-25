import Control.Monad (replicateM)
import Data.List (intercalate)

main :: IO ()
main = replicateM 2 getLine >>= putStrLn . findPrefix

findPrefix :: [String] -> String
findPrefix = showResult . findPrefix'

findPrefix' :: [String] -> [(Int, String)]
findPrefix' ["",ys] = [(0, ""), (0, ""), (length ys, ys)]
findPrefix' [xs,""] = [(0, ""), (length xs, xs), (0, "")]
findPrefix' [str1@(x:xs),str2@(y:ys)] =
  if x == y
  then
    let [(prefixLen, prefix), (xsLen, xs'), (ysLen, ys')] = findPrefix' [xs, ys]
    in [(prefixLen+1, (x:prefix)), (xsLen, xs'), (ysLen, ys')]
  else [(0, ""), (length str1, str1), (length str2, str2)]

showResult :: [(Int, String)] -> String
showResult = intercalate "\n" . map (\(len, str) -> show len ++ " " ++ str)

