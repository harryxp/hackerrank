import Data.Char
import Data.List

main :: IO ()
main =  getLine >>= \ln ->
  let nk = words ln
      nStr = head nk
      (n:k:_) = map read nk
  in
    (putStrLn . show . superDigit) (superDigit n * k)

superDigit     :: Integer -> Integer
superDigit n =
  let nStr = show n
  in if length nStr == 1 then n else (superDigit . sum . map (toInteger . digitToInt)) nStr

