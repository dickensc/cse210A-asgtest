module Bool where

import           Arith
import           General

import           Control.Applicative
import           System.Environment
import           Text.ParserCombinators.ReadP

-- ASTs --
data BooleanExpression = BooleanLiteral Bool
    | NotExpression BooleanExpression
    | EquivExpression ArithExpression ArithExpression
    | LessThanExpression ArithExpression ArithExpression
    | AndExpression BooleanExpression BooleanExpression
    | OrExpression BooleanExpression BooleanExpression

instance Show BooleanExpression where
  show (BooleanLiteral n)         = show n
  show (NotExpression b)          = show "¬" ++ show b
  show (EquivExpression e1 e2)    = "(" ++ show e1 ++ " == " ++ show e2 ++ ")"
  show (LessThanExpression e1 e2) = "(" ++ show e1 ++ " < " ++ show e2 ++ ")"
  show (AndExpression b1 b2)      = "(" ++ show b1 ++ " ∧ " ++ show b2 ++ ")"
  show (OrExpression b1 b2)       = "(" ++ show b1 ++ " ∨ " ++ show b2 ++ ")"

boolEval :: BooleanExpression -> Bool
boolEval (BooleanLiteral b)         = b
boolEval (NotExpression b)          = not (boolEval b)
boolEval (EquivExpression e1 e2)    = (arithEval e1) == (arithEval e2)
boolEval (LessThanExpression e1 e2) = (arithEval e1) < (arithEval e2)
boolEval (AndExpression b1 b2)      = (boolEval b1) && (boolEval b2)
boolEval (OrExpression b1 b2)       = (boolEval b1) || (boolEval b2)

-- Boolean parser --

-- reads the integer expression --
boolLiteral :: ReadP [Char]
boolLiteral = do
  consumeWhiteSpace
  literal <- string "true" <|> string "false" <|> string "TRUE" <|> string "FALSE"
  consumeWhiteSpace
  if literal == "true" || literal == "TRUE" then
    return "True"
  else
    return "False"

parseLiteralExpression :: ReadP BooleanExpression
parseLiteralExpression = do
  parsedBool <- boolLiteral
  return (BooleanLiteral (read parsedBool :: Bool))

parseNotExpression :: ReadP BooleanExpression
parseNotExpression = do
  consumeWhiteSpace
  char '¬'
  consumeWhiteSpace
  remainingExp <- orBool +++ brackets parseBool
  return (NotExpression remainingExp)

parseLessThanExpression :: ReadP BooleanExpression
parseLessThanExpression = do
  expr1 <- parseArith
  char '<'
  expr2 <- parseArith
  return (expr1 `LessThanExpression` expr2)

parseEquivExpression :: ReadP BooleanExpression
parseEquivExpression = do
  expr1 <- parseArith
  char '='
  expr2 <- parseArith
  return (expr1 `EquivExpression` expr2)

parseOrExpression :: ReadP BooleanExpression
parseOrExpression = do
  expr <- parseAndExpression +++ brackets parseBool
  char '∨'
  remainingExp <- orBool +++ brackets parseBool
  return (expr `OrExpression` remainingExp)

parseAndExpression :: ReadP BooleanExpression
parseAndExpression = do
  expr <- parseLiteralExpression +++ parseEquivExpression +++ parseLessThanExpression +++ brackets parseBool
  char '∧'
  remainingExp <- andBool +++ brackets parseBool
  return (expr `AndExpression` remainingExp)

parseBool :: ReadP BooleanExpression
parseBool = orBool

andBool :: ReadP BooleanExpression
andBool = parseLiteralExpression +++ parseNotExpression +++ parseEquivExpression +++ parseLessThanExpression +++ brackets parseBool +++ parseAndExpression

orBool :: ReadP BooleanExpression
orBool = andBool +++ parseOrExpression
