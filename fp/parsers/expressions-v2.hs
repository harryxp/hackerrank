import Data.List (intercalate)
import Text.ParserCombinators.ReadP ((<++),ReadP,eof,many1,readP_to_S,satisfy,skipSpaces)

class Evaluable t where
  eval :: t -> Int

data Expression = ExprAdd Term Expression
                | ExprSub Term Expression
                | ExprTerm Term

instance Show Expression where
  show (ExprAdd t e) = intercalate "" ["+\n", indent t, indent e]
  show (ExprSub t e) = intercalate "" ["-\n", indent t, indent e]
  show (ExprTerm t) = show t

data Term = TermMul Factor Term
          | TermDiv Factor Term
          | TermFactor Factor

instance Show Term where
  show (TermMul f t) = intercalate "" ["*\n", indent f, indent t]
  show (TermDiv f t) = intercalate "" ["/\n", indent f, indent t]
  show (TermFactor f) = show f

data Factor = FactorNumber Int
            | FactorPositive Factor
            | FactorNegative Factor
            | FactorExpression Expression

instance Show Factor where
  show (FactorNumber n) = show n ++ "\n"
  show (FactorPositive f) = show f
  show (FactorNegative f) = "-\n" ++ indent f
  show (FactorExpression e) = show e

indent :: Show a => a -> String
indent = unlines . map ("  " ++) . lines . show

p = 1000000007

main :: IO ()
--main = getLine >>= print . parseExpr
main = getLine >>= print . (`mod` p) . eval . parseExpr

-- Parser

parseExpr :: String -> Expression
parseExpr s = case filter (null . snd) (readP_to_S allP s) of
  [(e,"")] -> e
  otherwise -> error "Can't parse the expression."

allP :: ReadP Expression
allP = exprP >>= \e -> eof >> return e

exprP :: ReadP Expression
exprP = exprAddP <++ exprSubP <++ exprTermP

exprAddP :: ReadP Expression
exprAddP = binaryOpP '+' termP exprP ExprAdd

exprSubP :: ReadP Expression
exprSubP = binaryOpP '-' termP exprP ExprSub

exprTermP :: ReadP Expression
exprTermP = do
  t <- termP
  (return . ExprTerm) t

termP :: ReadP Term
termP = termMulP <++ termDivP <++ termFactorP

termMulP :: ReadP Term
termMulP = binaryOpP '*' factorP termP TermMul

termDivP :: ReadP Term
termDivP = binaryOpP '/' factorP termP TermDiv

termFactorP :: ReadP Term
termFactorP = do
  f <- factorP
  (return . TermFactor) f

factorP :: ReadP Factor
factorP = factorNumberP <++ factorPositiveP <++ factorNegative <++ factorExpressionP

factorNumberP :: ReadP Factor
factorNumberP = do
  digits <- (many1 digitP) :: ReadP String
  (return . FactorNumber . read) digits
  where
    digitP :: ReadP Char
    digitP = satisfy (\c -> c >= '0' && c <= '9')

factorPositiveP :: ReadP Factor
factorPositiveP = do
  satisfy (=='+')
  skipSpaces
  f <- factorP
  (return . FactorPositive) f

factorNegative :: ReadP Factor
factorNegative = do
  satisfy (=='-')
  skipSpaces
  f <- factorP
  (return . FactorNegative) f

factorExpressionP :: ReadP Factor
factorExpressionP = do
  satisfy (=='(')
  skipSpaces
  e <- exprP
  skipSpaces
  satisfy (==')')
  (return . FactorExpression) e

binaryOpP :: Char -> (ReadP a) -> (ReadP b) -> (a -> b -> c) -> ReadP c
binaryOpP c ra rb cons = do
  a <- ra
  skipSpaces
  satisfy (==c)
  skipSpaces
  b <- rb
  return (cons a b)

-- Evaluator

instance Evaluable Expression where
  eval (ExprAdd t e) = (eval t + eval e) `mod` p
  eval (ExprSub t e) = (eval t - eval e) `mod` p
  eval (ExprTerm t) = eval t `mod` p

instance Evaluable Term where
  eval (TermMul f t) = (eval f * eval t) `mod` p
  eval (TermDiv f t) = ((eval f `mod` p) * modPow (eval t `mod` p) (p-2)) `mod` p
  eval (TermFactor f) = eval f `mod` p

modPow :: Int -> Int -> Int
modPow _ 0 = 1
modPow b e = let x = modPow b (e `div` 2) ^ 2 `mod` p in
  if even e then x else (x * b) `mod` p

instance Evaluable Factor where
  eval (FactorNumber n) = n `mod` p
  eval (FactorPositive f) = eval f `mod` p
  eval (FactorNegative f) = -(eval f) `mod` p
  eval (FactorExpression e) = eval e `mod` p

