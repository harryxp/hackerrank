import Text.ParserCombinators.ReadP

main :: IO ()
main = getLine >> getContents >>= mapM print . map (simplify . filter (/=' ')) . lines >> return ()

data Expression = ExprAdd Term Expression
                | ExprSub Term Expression
                | ExprTerm Term

data Term = TermMul Factor Term
          | TermDiv Factor Term
          | TermFactor Factor

data Factor = FactorSimple { hasX :: Bool
                           , coefficient :: Int
                           , exponent :: Int
                           }
            | FactorExpression Expression

simplify :: String -> Expression
simplify = realSimplify . parse

parse :: String -> Expression
parse s = case filter (null . snd) (readP_to_S allP s) of
  [(e,"")] -> e
  otherwise -> error "Can't parse the expression."

allP :: ReadP Expression
allP = exprP >>= \e -> eof >> return e

exprP :: ReadP Expression
exprP = do
  t <- termP
  op <- option ' ' (char '+' +++ char '-')
  case op of
    '+' -> exprP >>= return . ExprAdd t
    '-' -> exprP >>= return . ExprSub t
    otherwise -> (return . ExprTerm) t

termP :: ReadP Term
termP = do
  sign <- option '+' (char '-' +++ char '+')
  t <- termExprP (sign=='+') +++ termSimpleP (sign=='+')
  return t

termSimpleP :: Bool -> ReadP Term
termSimpleP sign = xOnlyP sign <++ coefXP sign <++ coefOnlyP sign

xOnlyP :: Bool -> ReadP Term
xOnlyP sign = char 'x' >> expoP >>= \ex -> return TermSimple { sign = sign, coefficient = 1, hasX = True, expo = ex}

coefOnlyP :: Bool -> ReadP Term
coefOnlyP sign = do
  digits <- digitsP
  ex <- expoP
  return TermSimple { sign = sign
                    , coefficient = digits
                    , hasX = False
                    , expo = ex
                    }

coefXP :: Bool -> ReadP Term
coefXP sign = do
  digits <- digitsP
  char 'x'
  ex <- expoP
  return TermSimple { sign = sign
                    , coefficient = digits
                    , hasX = True
                    , expo = ex
                    }

expoP :: ReadP Int
expoP = option 1 (char '^' >> digitsP)

termExprP :: Bool -> ReadP Term
termExprP sign = between (char '(') (char ')') exprP >>= return . TermExpr sign

digitsP :: ReadP Int
digitsP = munch1 isDigit >>= return . read
  where
    isDigit :: Char -> Bool
    isDigit c = c >= '0' && c <= '9'

realSimplify :: Expression -> Expression
realSimplify = undefined
