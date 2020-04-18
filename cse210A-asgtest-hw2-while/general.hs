module General where

import           Control.Applicative
import           Control.Monad.State
import           Data.Char
import qualified Data.Map                     as Map
import           System.Environment
import           Text.ParserCombinators.ReadP


-- State Operations --


type ProgramState = Map.Map String Int

insertStateVariable :: String -> Int -> State ProgramState ()
insertStateVariable name value = state $ \pgState -> ((), Map.insert name value pgState)

getStateValue :: String -> State ProgramState Int
getStateValue name = do
  stateNow <- Control.Monad.State.get
  return $ stateNow Map.! name

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


-- parsers for variables --

character :: ReadP Char
character =
  satisfy isLetter <|> satisfy isNumber

atLeastOneCharacter :: ReadP [Char]
atLeastOneCharacter = many1 character

parseVariableExpression :: ReadP [Char]
parseVariableExpression = do
  consumeWhiteSpace
  variableName <- atLeastOneCharacter
  consumeWhiteSpace
  return variableName
