{-|
  Razor
  Module      : REPL.Main
  Description : The module provides a REPL for user interaction with Razor.
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module Main where
import API.Surface
import REPL.Display
import Common.Model
import Common.Provenance
import Data.Maybe
import Data.List
import Syntax.GeometricUtils 
import SAT.Impl
import System.Console.Haskeline
import Chase.Impl
import System.Environment
import Tools.Config
import Control.Monad.Trans

import REPL.Mode
import qualified REPL.Mode.Theory as T

data REPLMode = TheoryMode T.TheoryM

main :: IO ()
main = do
  -- init display
  displayInit
  -- get configuration
  config <- getConfig 
  {--- get the start state
  startState <- getStartState config
  case startState of
    Left (UErr err) -> error err
    Right state@(UState (cfg, thy) (b,p,t,_) (stream, mdl)) -> do
      (prettyPrint 0 flow (show mdl))
      runInputT defaultSettings (loop state)-}

  -- enter starting mode
  let state = REPLState config Nothing Nothing Nothing
  let startmode = T.TheoryM
  enter <- enterMode startmode state
  case enter of
    Left err -> prettyPrint 0 ferror err
    Right state' -> runInputT defaultSettings $ loop state' startmode
  -- exit display
  displayExit

loop :: LoopMode m => REPLState -> m -> InputT IO()
loop state mode = do
  -- loop types
  let stay = loop state mode
  let go state' = loop state' mode
  -- get input
  minput <- getInputLine "% "
  -- parse input into a command and act depending on the case
  case minput of
      Nothing -> return ()
      Just command -> do
        run <- lift $ runOnce mode state command
        case run of
          Left err -> do
            lift $ prettyPrint 0 ferror err
            stay
          Right state' -> go state'

{-loop :: UState -> InputT IO ()
loop state@(UState (cfg, thy) (b,p,t,_) (stream, mdl)) = do
  -- possible REPL loops
  let sameLoop = loop state
  let newLoop state'@(UState _ _ (stream', mdl')) = (lift $ prettyPrint 0 flow (show mdl')) >> loop state'
  -- get input
  minput <- getInputLine "% "
  -- parse input into a command and act depending on the case
  case minput of
      Nothing -> return ()
      Just command -> case (parseCommand command) of
        -- display
        Display thing -> case thing of
          DispTheory -> (lift $ mapM_ (\s-> prettyPrint 0 finput ((show s)++"\n")) thy) >> sameLoop
          DispModel -> newLoop state
        -- exploration
        Go explore -> case explore of
          Next -> case getNextModel state of
            Left (UErr err) -> (lift $ prettyPrint 0 ferror (err++"\n")) >> sameLoop
            Right state' -> newLoop state'
          Augment fml -> case getAugmentedState state fml of
            Left (UErr err) -> (lift $ prettyPrint 0 ferror (err++"\n")) >> sameLoop
            Right state' -> newLoop state'
        -- explanation
        Ask question -> case question of
          Name isall isrec term -> do
            let origins = getOrigin state isrec term
            if isall
              then lift $ mapM_ (printOrigin thy 0) origins
              else lift $ printOrigin thy 0 (head origins)
            sameLoop
          Blame atom -> do
            let justification = getJustification state atom
            lift $ printJustification atom thy justification
            sameLoop
        -- others
        Other utility -> case utility of
          Help -> (lift $ prettyPrint 0 foutput helpCommand) >> sameLoop
          Exit -> (lift $ prettyPrint 0 foutput "closing...\n") >> return ()
        SyntaxError err -> (lift $ prettyPrint 0 ferror (err++"\n")) >> sameLoop

printOrigin :: Theory -> Int -> UOrigin -> IO ()
printOrigin thy tabs (UOriginLeaf term origin) = do
  prettyPrint tabs foutput ("origin of "++(show term)++"\n")
  printBlame thy origin tabs (show term)
printOrigin thy tabs (UOriginNode term origin depends) = do
  printOrigin thy tabs (UOriginLeaf term origin)
  mapM_ (printOrigin thy (tabs+1)) depends

printJustification :: Formula -> Theory -> UBlame -> IO()
printJustification atom thy justification = do 
  prettyPrint 0 foutput ("justification of "++(show atom)++"\n")
  printBlame thy justification 0 (show atom)
    

printBlame :: Theory -> UBlame -> Int -> String -> IO()
printBlame thy blame tabs highlight = case blame of
  Left (UErr err) -> prettyPrint tabs ferror (err++"\n")
  Right ((TheoryBlame i sub), blamed) -> printDiff (thy !! (i-1)) blamed (highlight,tabs)
  Right ((UserBlame augmentation), blamed) -> prettyPrint tabs finput ("user augmentation: "++(show augmentation)++"\n")

printDiff :: Sequent -> Sequent -> (String, Int) -> IO()
printDiff original diff format@(highlight, tabs) = do
  prettyPrint tabs finput ("thy rule: "++(show original)++"\n")
  prettyHighlight tabs highlight ("instance: "++(show diff)++"\n")
-}