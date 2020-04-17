import           Arith
import           Bool
import           Text.ParserCombinators.ReadP

parseWhile :: ReadP BooleanExpression
parseWhile = parseBool

-- Driver --
main = do
  inputString <- getContents
  let parsings = readP_to_S parseBool inputString
      keptParsing = last parsings
      expression = fst keptParsing
      evaluation = boolEval expression
  putStrLn (show evaluation)
