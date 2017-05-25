import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP (ReadP,many1,readP_to_S,satisfy)

class Evaluable t where
  eval :: t -> Integer

data Expression = ExprAdd Term Expression
                | ExprSub Term Expression
                | ExprTerm Term

data Term = TermMul Factor Term
          | TermDiv Factor Term
          | TermFactor Factor

data Factor = FactorNumber Integer
            | FactorPositive Factor
            | FactorNegative Factor
            | FactorExpression Expression

main :: IO ()
main = getLine >>= print . (`mod` 1000000007) . eval . parseExpr

-- TODO spaces
-- TODO right associative

-- Parser

parseExpr :: String -> Expression
parseExpr s = case readP_to_S exprP s of
  [(expr,"")] -> expr
  otherwise -> error "Can't parse the expression."

exprP :: ReadP Expression
exprP = exprAddP <|> exprSubP <|> exprTermP

exprAddP :: ReadP Expression
exprAddP = binaryOpP '+' termP exprP ExprAdd

exprSubP :: ReadP Expression
exprSubP = binaryOpP '-' termP exprP ExprSub

exprTermP :: ReadP Expression
exprTermP = do
  t <- termP
  (return . ExprTerm) t

termP :: ReadP Term
termP = termMulP <|> termDivP <|> termFactorP

termMulP :: ReadP Term
termMulP = binaryOpP '*' factorP termP TermMul

termDivP :: ReadP Term
termDivP = binaryOpP '/' factorP termP TermDiv

termFactorP :: ReadP Term
termFactorP = do
  f <- factorP
  (return . TermFactor) f

factorP :: ReadP Factor
factorP = factorNumberP <|> factorPositiveP <|> factorNegative <|> factorExpressionP

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
  f <- factorP
  (return . FactorPositive) f

factorNegative :: ReadP Factor
factorNegative = do
  satisfy (=='-')
  f <- factorP
  (return . FactorNegative) f

factorExpressionP :: ReadP Factor
factorExpressionP = do
  satisfy (=='(')
  e <- exprP
  satisfy (==')')
  (return . FactorExpression) e

binaryOpP :: Char -> (ReadP a) -> (ReadP b) -> (a -> b -> c) -> ReadP c
binaryOpP c ra rb cons = do
  a <- ra
  satisfy (==c)
  b <- rb
  return (cons a b)

-- Evaluator

instance Evaluable Expression where
  eval (ExprAdd t e) = eval t + eval e
  eval (ExprSub t e) = eval t - eval e
  eval (ExprTerm t) = eval t

instance Evaluable Term where
  eval (TermMul f t) = eval f * eval t
  eval (TermDiv f t) = eval f `div` eval t  -- TODO
  eval (TermFactor f) = eval f

instance Evaluable Factor where
  eval (FactorNumber n) = n
  eval (FactorPositive f) = eval f
  eval (FactorNegative f) = -(eval f)
  eval (FactorExpression e) = eval e
