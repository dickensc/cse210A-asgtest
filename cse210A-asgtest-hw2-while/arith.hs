module Arith where

import           General

import           Control.Applicative
import           Control.Monad.State
import           System.Environment
import           Text.ParserCombinators.ReadP

-- ASTs --
data ArithExpression = IntExpression Int
    | VarExpression String
    | SumExpression ArithExpression ArithExpression
    | SubtractionExpression ArithExpression ArithExpression
    | MulExpression ArithExpression ArithExpression
    | ExpExpression ArithExpression ArithExpression

instance Show ArithExpression where
  show (IntExpression n)     = show n
  show (VarExpression x)     = show x
  show (SumExpression e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (SubtractionExpression e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (MulExpression e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (ExpExpression e1 e2) = "(" ++ show e1 ++ " ^ " ++ show e2 ++ ")"

-- Variables do not need to be declared. A reference to an unset variable returns 0. --
arithEval :: ArithExpression -> State ProgramState Int
arithEval (IntExpression n)     = state $ \pgState -> (n, pgState)
arithEval (VarExpression x)     = getStateValue x
arithEval (SumExpression e1 e2) = do
  s1 <- arithEval e1
  s2 <- arithEval e2
  return (s1 + s2)
arithEval (SubtractionExpression e1 e2) = do
  s1 <- arithEval e1
  s2 <- arithEval e2
  return (s1 - s2)
arithEval (MulExpression e1 e2) = do
  s1 <- arithEval e1
  s2 <- arithEval e2
  return (s1 * s2)
arithEval (ExpExpression e1 e2) = do
  s1 <- arithEval e1
  s2 <- arithEval e2
  return (s1 ^ s2)

-- Arith parser --

-- reads the integer expression --
integerExpression :: ReadP [Char]
integerExpression = do
  consumeWhiteSpace
  expression <- atLeastOneNumber
  consumeWhiteSpace
  return expression

parseIntExpression :: ReadP ArithExpression
parseIntExpression = do
  parsedInteger <- integerExpression
  return (IntExpression (read parsedInteger :: Int))

variableName :: ReadP [Char]
variableName = do
  consumeWhiteSpace
  name <- atLeastOneCharacter
  consumeWhiteSpace
  return name

parseVariableExpression :: ReadP ArithExpression
parseVariableExpression = do
  parsedVarName <- variableName
  return (VarExpression parsedVarName)

parseSumExpression :: ReadP ArithExpression
parseSumExpression = do
  expr <- mulArith +++ brackets parseArith
  char '+'
  remainingExp <- parseArith
  return (expr `SumExpression` remainingExp)

parseSubExpression :: ReadP ArithExpression
parseSubExpression = do
  expr <- mulArith +++ brackets parseArith
  char '-'
  remainingExp <- parseArith
  return (expr `SubtractionExpression` remainingExp)

parseMulExpression :: ReadP ArithExpression
parseMulExpression = do
  expr <- expArith +++ brackets parseArith
  char '*'
  remainingExp <- mulArith +++ brackets parseArith
  return (expr `MulExpression` remainingExp)

parseExpExpression :: ReadP ArithExpression
parseExpExpression = do
  expr <- parseVariableExpression +++ parseIntExpression +++ brackets parseArith
  char '^'
  remainingExp <- expArith +++ brackets parseArith
  return (expr `ExpExpression` remainingExp)

parseArith :: ReadP ArithExpression
parseArith = sumArith +++ subArith

expArith :: ReadP ArithExpression
expArith = parseVariableExpression +++ parseIntExpression +++ brackets parseArith +++ parseExpExpression

mulArith :: ReadP ArithExpression
mulArith = expArith +++ parseMulExpression

sumArith :: ReadP ArithExpression
sumArith = mulArith +++ parseSumExpression

subArith :: ReadP ArithExpression
subArith = mulArith +++ parseSubExpression
