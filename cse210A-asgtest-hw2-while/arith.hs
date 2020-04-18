module Arith where

import           General

import           Control.Applicative
import           System.Environment
import           Text.ParserCombinators.ReadP

-- ASTs --
data ArithExpression = IntExpression Int
    | VarExpression String
    | SumExpression ArithExpression ArithExpression
    | MulExpression ArithExpression ArithExpression
    | ExpExpression ArithExpression ArithExpression

instance Show ArithExpression where
  show (IntExpression n)     = show n
  show (VarExpression n)     = show n
  show (SumExpression e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (MulExpression e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (ExpExpression e1 e2) = "(" ++ show e1 ++ " ^ " ++ show e2 ++ ")"

-- Variables do not need to be declared. A reference to an unset variable returns 0. --
arithEval :: ArithExpression -> Int
arithEval (IntExpression n)     = n
arithEval (SumExpression e1 e2) = (arithEval e1) + (arithEval e2)
arithEval (MulExpression e1 e2) = (arithEval e1) * (arithEval e2)
arithEval (ExpExpression e1 e2) = (arithEval e1) ^ (arithEval e2)

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

variableExpression :: ReadP [Char]
variableExpression = do
  consumeWhiteSpace
  expression <- atLeastOneNumber
  consumeWhiteSpace
  return expression

parseVariableExpression :: ReadP ArithExpression
parseVariableExpression = do
  parsedInteger <- integerExpression
  return (VarExpression (read parsedInteger :: Int))

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
expArith = parseVariableExpression ++ parseIntExpression +++ brackets parseArith +++ parseExpExpression

mulArith :: ReadP ArithExpression
mulArith = expArith +++ parseMulExpression

sumArith :: ReadP ArithExpression
sumArith = mulArith +++ parseSumExpression
