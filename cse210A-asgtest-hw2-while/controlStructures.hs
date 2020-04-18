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
    | WhileStructure BooleanExpression ControlStructure
    | OrderStructure ControlStructure ControlStructure
    | SkipStructure

instance Show ControlStructure where
  show (AssignmentStructure s n) = s ++ " := " ++ show n
  show (IfStructure b c1 c2)         = "if " ++ show b ++ " then " ++ show c1 ++ " else " ++ show c2
  show (WhileStructure b c)      = "while " ++ show b ++ " do " ++ show c
  show (OrderStructure c1 c2) = show c1 ++ "; " ++ show c2
  show (SkipStructure)      = "skip "

controlEval :: ControlStructure -> State ProgramState ()
controlEval (AssignmentStructure s arithExp)     = do
  expr <- arithEval arithExp
  insertStateVariable s expr
  return ()
controlEval (IfStructure b c1 c2) = do
  evalualtedBool <- boolEval b
  if evalualtedBool
  then controlEval c1
  else
       controlEval c2
  return ()
controlEval (WhileStructure b c) = do
  evaluatedBool <- boolEval b
  if evaluatedBool
  then do
     controlEval c
     controlEval ( WhileStructure b c )
  else
      controlEval SkipStructure
  return ()
controlEval (OrderStructure c1 c2) = do
  controlEval c1
  controlEval c2
  return ()
controlEval SkipStructure = return ()

-- Control Parser --
parseAssignmentStructure :: ReadP ControlStructure
parseAssignmentStructure = do
  consumeWhiteSpace
  variableName <- atLeastOneCharacter
  consumeWhiteSpace
  string ":="
  consumeWhiteSpace
  arithmeticExpression <- parseArith
  return (variableName `AssignmentStructure` arithmeticExpression)

parseOrderedStructure :: ReadP ControlStructure
parseOrderedStructure = do
  expr <- parseAssignmentStructure +++ parseIfStructure
  char ';'
  remainingExp <- parseControl
  return (expr `OrderStructure` remainingExp)

parseIfStructure :: ReadP ControlStructure
parseIfStructure = do
  consumeWhiteSpace
  string "if"
  consumeWhiteSpace
  boolExpr <- parseBool
  consumeWhiteSpace
  string "then"
  consumeWhiteSpace
  ifBlock <- parseControl
  consumeWhiteSpace
  string "else"
  consumeWhiteSpace
  elseBlock <- parseControl
  return (IfStructure boolExpr ifBlock elseBlock)

orderedStructure :: ReadP ControlStructure
orderedStructure = parseOrderedStructure

ifStructure :: ReadP ControlStructure
ifStructure = orderedStructure +++ parseIfStructure

assignmentStructure :: ReadP ControlStructure
assignmentStructure = ifStructure +++ parseAssignmentStructure

parseControl :: ReadP ControlStructure
parseControl = assignmentStructure
