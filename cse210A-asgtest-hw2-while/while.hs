import           Arith
import           Bool
import           Control.Monad.State
import           ControlStructures
import           Data.List
import qualified Data.Map                     as Map
import           Text.ParserCombinators.ReadP


parseWhile :: ReadP ControlStructure
parseWhile = parseControl

-- Driver --
main = do
  inputString <- getContents
  let parsings = readP_to_S parseWhile inputString
      keptParsing = last parsings
      expression = fst keptParsing
      evaluation = runState (controlEval expression) Map.empty
      finalState = snd evaluation
      formattedFinalState = concat (intersperse ", " [x ++ " → " ++ show val | (x,val) <- (Map.assocs finalState)])
  putStrLn ("{" ++ formattedFinalState ++ "}" )
