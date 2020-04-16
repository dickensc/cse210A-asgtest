import           Control.Applicative
import           System.Environment
import           Text.ParserCombinators.ReadP

-- A parser for whiteSpace --
isWhiteSpace :: Char -> Bool
isWhiteSpace char =
  any (char ==) " "

whiteSpace :: ReadP Char
whiteSpace =
  satisfy isWhiteSpace

consumeWhiteSpace :: ReadP [Char]
consumeWhiteSpace =
  Text.ParserCombinators.ReadP.many whiteSpace

-- parsers for numerics --
isNumber :: Char -> Bool
isNumber char =
  any (char ==) "0123456789"

number :: ReadP Char
number =
  satisfy isNumber

sign :: ReadP Char
sign =
    satisfy  (== '-')

atLeastOneNumber :: ReadP [Char]
atLeastOneNumber = do
  numSign <- Text.ParserCombinators.ReadP.option '+' sign
  num <- many1 number
  if numSign == '+' then
    return num
  else
    return ("-" ++ num)

-- reads the integer expression --
integerExpression :: ReadP [Char]
integerExpression = do
  consumeWhiteSpace
  expression <- atLeastOneNumber
  consumeWhiteSpace
  return expression

-- ASTs --
data ArithExpression = IntExpression Int
    | SumExpression ArithExpression ArithExpression
    | MulExpression ArithExpression ArithExpression
    | ExpExpression ArithExpression ArithExpression

instance Show ArithExpression where
  show (IntExpression n)     = show n
  show (SumExpression e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (MulExpression e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (ExpExpression e1 e2) = "(" ++ show e1 ++ " ^ " ++ show e2 ++ ")"

eval :: ArithExpression -> Int
eval (IntExpression n)     = n
eval (SumExpression e1 e2) = (eval e1) + (eval e2)
eval (MulExpression e1 e2) = (eval e1) * (eval e2)
eval (ExpExpression e1 e2) = (eval e1) ^ (eval e2)

-- Arith parser --
brackets :: ReadP a -> ReadP a
brackets p = do
  consumeWhiteSpace
  char '('
  consumeWhiteSpace
  r <- p
  consumeWhiteSpace
  char ')'
  consumeWhiteSpace
  return r

parseIntExpression :: ReadP ArithExpression
parseIntExpression = do
  parsedInteger <- integerExpression
  return (IntExpression (read parsedInteger :: Int))

parseSumExpression :: ReadP ArithExpression
parseSumExpression = do
  expr <- mulArith +++ brackets parseArith
  char '+'
  remainingExp <- sumArith
  return (expr `SumExpression` remainingExp)

parseMulExpression :: ReadP ArithExpression
parseMulExpression = do
  expr <- expArith +++ brackets parseArith
  char '*'
  remainingExp <- mulArith +++ brackets parseArith
  return (expr `MulExpression` remainingExp)

parseExpExpression :: ReadP ArithExpression
parseExpExpression = do
  expr <- parseIntExpression +++ brackets parseArith
  char '^'
  remainingExp <- expArith +++ brackets parseArith
  return (expr `ExpExpression` remainingExp)

parseArith :: ReadP ArithExpression
parseArith = sumArith

expArith :: ReadP ArithExpression
expArith = parseIntExpression +++ brackets parseArith +++ parseExpExpression

mulArith :: ReadP ArithExpression
mulArith = expArith +++ parseMulExpression

sumArith :: ReadP ArithExpression
sumArith = mulArith +++ parseSumExpression

-- Driver --
main = do
  inputString <- getContents
  let parsings = readP_to_S parseArith inputString
      keptParsing = last parsings
      expression = fst keptParsing
      evaluation = eval expression
  putStrLn (show evaluation)
