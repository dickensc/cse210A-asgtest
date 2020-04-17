module General where

import           Control.Applicative
import           System.Environment
import           Text.ParserCombinators.ReadP


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
