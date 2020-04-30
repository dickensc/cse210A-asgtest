module Bool where

import           Arith
import           General

import           Control.Applicative
import           Control.Monad.State
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
  show (BooleanLiteral True)    = "true"
  show (BooleanLiteral False)   = "false"
  show (EquivExpression a b)    = "(" ++ show a ++ "=" ++ show b ++ ")"
  show (LessThanExpression a b) = "(" ++ show a ++ "<" ++ show b ++ ")"
  show (NotExpression a)        = "¬" ++ show a
  show (OrExpression a b)       = "(" ++ show a ++ "∨" ++ show b ++ ")"
  show (AndExpression a b)      = "(" ++ show a ++ "∧" ++ show b ++ ")"

boolEval :: BooleanExpression -> State ProgramState Bool
boolEval (BooleanLiteral b)         = state $ \pgState -> (b, pgState)
boolEval (NotExpression b)          = do
  b1 <- boolEval b
  return (not b1)
boolEval (EquivExpression e1 e2)    = do
  s1 <- arithEval e1
  s2 <- arithEval e2
  return (s1 == s2)
boolEval (LessThanExpression e1 e2) = do
  s1 <- arithEval e1
  s2 <- arithEval e2
  return (s1 < s2)
boolEval (AndExpression b1 b2)      = do
  bool1 <- boolEval b1
  bool2 <- boolEval b2
  return (bool1 && bool2)
boolEval (OrExpression b1 b2)       = do
  bool1 <- boolEval b1
  bool2 <- boolEval b2
  return (bool1 || bool2)

-- Boolean parser --

-- reads the integer expression --
boolLiteral :: ReadP [Char]
boolLiteral = do
  literal <- string "true" <|> string "false" <|> string "TRUE" <|> string "FALSE"
  consumeWhiteSpaceOpt
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
  char '¬'
  consumeWhiteSpaceMandatory
  remainingExp <- brackets parseBool +++ parseLiteralExpression
  return (NotExpression remainingExp)

parseLessThanExpression :: ReadP BooleanExpression
parseLessThanExpression = do
  expr1 <- parseArith
  char '<'
  consumeWhiteSpaceMandatory
  expr2 <- parseArith
  return (expr1 `LessThanExpression` expr2)

parseEquivExpression :: ReadP BooleanExpression
parseEquivExpression = do
  expr1 <- parseArith
  char '='
  consumeWhiteSpaceMandatory
  expr2 <- parseArith
  return (expr1 `EquivExpression` expr2)

parseOrExpression :: ReadP BooleanExpression
parseOrExpression = do
  expr <- andBool
  char '∨'
  consumeWhiteSpaceMandatory
  remainingExp <- orBool
  return (expr `OrExpression` remainingExp)

parseAndExpression :: ReadP BooleanExpression
parseAndExpression = do
  expr <- parseLiteralExpression +++ parseNotExpression +++ parseEquivExpression +++ parseLessThanExpression +++ brackets parseBool
  char '∧'
  consumeWhiteSpaceMandatory
  remainingExp <- andBool
  return (expr `AndExpression` remainingExp)

parseBool :: ReadP BooleanExpression
parseBool = orBool

andBool :: ReadP BooleanExpression
andBool = parseLiteralExpression +++ parseNotExpression +++ parseEquivExpression +++ parseLessThanExpression +++ brackets parseBool +++ parseAndExpression

orBool :: ReadP BooleanExpression
orBool = andBool +++ parseOrExpression
