import           Arith
import           Bool
import           Control.Monad.State
import           ControlStructures
import           Data.List
import qualified Data.Map                     as Map
import           Text.ParserCombinators.ReadP


parseWhile :: ReadP ControlStructure
parseWhile = parseControl

formatState :: Map.Map String Int -> String
formatState s = "{" ++ concat (intersperse ", " [x ++ " → " ++ show val | (x,val) <- (Map.assocs s)]) ++ "}"

formatProgram :: ControlStructure -> String
formatProgram cs = "⇒ " ++ (show cs)

whileEval :: (ControlStructure, Map.Map String Int) -> String
whileEval (cs, state)
  | isSkip cs = ""
    -- let
    -- formattedFinalState = concat (intersperse ", " [x ++ " → " ++ show val | (x,val) <- (Map.assocs state)])
    -- in ("⇒ " ++ (show SkipStructure) ++ ", " ++ "{" ++ formattedFinalState ++ "}")
  | otherwise = let
    evaluation = runState (controlEval cs) state
    in (formatProgram (fst evaluation) ++ ", " ++ (formatState (snd evaluation)) ++ "\n" ++ (whileEval ((fst evaluation), (snd evaluation))))

-- whileEval :: (ControlStructure, Map.Map k a) -> String
-- whileEval (cs, s)= do
--   expression <- show cs
--   return expression
--   -- resultingState <- snd evaluation
--   -- formattedResultingState <- concat (intersperse ", " [x ++ " → " ++ show val | (x,val) <- (Map.assocs state)])
--   -- remainingStructure <- fst evaluation
--   -- return ("⇒ " ++ (show remainingStructure) ++ ", " ++ "{" ++ formattedResultingState ++ "}" ++ "\n" ++ (whileEval remainingStructure))


-- whileEval :: (ControlStructure, Map.Map k a) -> String
-- whileEval (cs, state)
--   | isSkip cs = do
--     formattedFinalState <- concat (intersperse ", " [x ++ " → " ++ show val | (x,val) <- (Map.assocs state)])
--     return ("⇒ " ++ (show SkipStructure) ++ ", " ++ "{" ++ formattedFinalState ++ "}")
--   | otherwise = do
--     evaluation <- runState (controlEval cs) state
--     resultingState <- snd evaluation
--     formattedResultingState <- concat (intersperse ", " [x ++ " → " ++ show val | (x,val) <- (Map.assocs state)])
--     remainingStructure <- fst evaluation
--     return ("⇒ " ++ (show remainingStructure) ++ ", " ++ "{" ++ formattedResultingState ++ "}" ++ "\n" ++ (whileEval remainingStructure resultingState))

-- Driver --
main = do
  inputString <- getContents
  let parsings = readP_to_S parseWhile inputString
      keptParsing = last parsings
      expression = fst keptParsing
  putStrLn (whileEval (expression, Map.empty))
