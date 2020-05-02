module ControlStructures where

import           Arith
import           Bool
import           General

import           Control.Applicative
import           Control.Monad.State
import           System.Environment
import           Text.ParserCombinators.ReadP



-- ASTs --
data ControlStructure = AssignmentStructure String ArithExpression
    | IfStructure BooleanExpression ControlStructure ControlStructure
    | TernaryStructure String BooleanExpression ArithExpression
                   ArithExpression
    | WhileStructure BooleanExpression ControlStructure
    | OrderStructure ControlStructure ControlStructure
    | SkipStructure

instance Show ControlStructure where
  show SkipStructure = "skip"
  show (AssignmentStructure s a) = s ++ " := " ++ show a
  show (OrderStructure a b) = show a ++ "; " ++ show b
  show (IfStructure c a b) = "if " ++ show c ++ " then { " ++ show a ++ " } else { " ++ show b ++ " }"
  show (WhileStructure b c) = "while " ++ show b ++ " do { " ++ show c ++ " }"
  show (TernaryStructure s b a1 a2)         = s ++ " := " ++ show b ++ " ? " ++ show a1 ++ " : " ++ show a2

controlEval :: ControlStructure -> State ProgramState (ControlStructure)
controlEval (AssignmentStructure s arithExp)     = do
  expr <- arithEval arithExp
  insertStateVariable s expr
  return SkipStructure
controlEval (IfStructure b c1 c2) = do
  evaluatedBool <- boolEval b
  if evaluatedBool
  then return c1
  else return c2
controlEval (TernaryStructure s b a1 a2) = do
  evaluatedBool <- boolEval b
  if evaluatedBool
  then return (AssignmentStructure s a1)
  else return (AssignmentStructure s a2)
controlEval (WhileStructure b c) = do
  evaluatedBool <- boolEval b
  if evaluatedBool
  then return (OrderStructure c (WhileStructure b c))
  else return SkipStructure
controlEval (OrderStructure c1 c2) = do
  one <- controlEval c1
  if (isSkip c1)
  then return c2
  else return (OrderStructure one c2)
controlEval SkipStructure = do
  return SkipStructure

isSkip :: ControlStructure -> Bool
isSkip SkipStructure = True
isSkip _ = False

-- Control Parser --
parseAssignmentStructure :: ReadP ControlStructure
parseAssignmentStructure = do
  variableName <- atLeastOneCharacter
  consumeWhiteSpaceMandatory
  string ":="
  consumeWhiteSpaceMandatory
  arithmeticExpression <- parseArith
  return (variableName `AssignmentStructure` arithmeticExpression)

parseOrderedStructure :: ReadP ControlStructure
parseOrderedStructure = do
  expr <- parseAssignmentStructure +++ parseIfStructure +++ parseWhileStructure +++ parseSkipStructure +++ curly parseControl
  char ';'
  consumeWhiteSpaceMandatory
  remainingExp <- parseControl +++ curly parseControl
  return (expr `OrderStructure` remainingExp)

parseIfStructure :: ReadP ControlStructure
parseIfStructure = do
  string "if"
  consumeWhiteSpaceMandatory
  boolExpr <- parseBool
  string "then"
  consumeWhiteSpaceMandatory
  ifBlock <- parseSkipStructure +++ parseAssignmentStructure +++ parseIfStructure +++ parseWhileStructure +++ curly parseControl
  string "else"
  consumeWhiteSpaceMandatory
  elseBlock <- parseSkipStructure +++ parseAssignmentStructure +++ parseIfStructure +++ parseWhileStructure +++ curly parseControl
  return (IfStructure boolExpr ifBlock elseBlock)

parseTernaryStructure :: ReadP ControlStructure
parseTernaryStructure = do
  variableName <- atLeastOneCharacter
  consumeWhiteSpaceMandatory
  string ":="
  consumeWhiteSpaceMandatory
  boolExpr <- parseBool
  string "?"
  consumeWhiteSpaceMandatory
  trueVal <- parseArith
  string ":"
  consumeWhiteSpaceMandatory
  falseVal <- parseArith
  return (TernaryStructure variableName boolExpr trueVal falseVal)

parseWhileStructure :: ReadP ControlStructure
parseWhileStructure = do
  string "while"
  consumeWhiteSpaceMandatory
  boolExpr <- parseBool
  string "do"
  consumeWhiteSpaceMandatory
  doBlock <- parseSkipStructure +++ parseAssignmentStructure +++ parseIfStructure +++ parseWhileStructure +++ curly parseControl
  return (WhileStructure boolExpr doBlock)

parseSkipStructure :: ReadP ControlStructure
parseSkipStructure = do
  string "skip"
  consumeWhiteSpaceOpt
  return (SkipStructure)

orderedStructure :: ReadP ControlStructure
orderedStructure = parseOrderedStructure +++ curly parseControl

whileStructure :: ReadP ControlStructure
whileStructure = orderedStructure +++ parseWhileStructure

ifStructure :: ReadP ControlStructure
ifStructure = whileStructure +++ parseIfStructure

ternaryStructure :: ReadP ControlStructure
ternaryStructure = ifStructure +++ parseTernaryStructure

assignmentStructure :: ReadP ControlStructure
assignmentStructure = ternaryStructure +++ parseAssignmentStructure

skipStructure :: ReadP ControlStructure
skipStructure = assignmentStructure +++ parseSkipStructure

parseControl :: ReadP ControlStructure
parseControl = skipStructure
