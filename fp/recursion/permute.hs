import Data.List (intercalate)

main :: IO ()
main =
  getLine >>=
  \numStrings -> mapM (\_ -> getLine) [1..read numStrings] >>=
  \strings -> (putStrLn . permute) strings

permute :: [String] -> String
permute strings = intercalate "\n" $ map permuteString strings

permuteString :: String -> String
permuteString (x:x':xs) = (x':x:permuteString xs)
permuteString [] = []
