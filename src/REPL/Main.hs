{-|
  Razor
  Module      : REPL.Main
  Description : The module provides a REPL for user interaction with Razor.
  Maintainer  : Salman Saghafi, Ryan Danas
-}
{-| TODO / BUGS
replace functions with elements (cardOf(e^3), replace with e^4)
how to deal with flattening... exists x. exists y. Q(x, y, f(g(x, y)))
allow user to give any expr that evaluates to an element
augmentation
-}
{-| PEDANDTIC / DISPLAY
the text output for larger theories should still be managable (e^0, e^11 is a lot for each element)
origin* should not print redundant info as it goes down the tree
-}
module Main where
import API.Surface
import API.UserSyntax
import REPL.Display
import Common.Model
import Common.Provenance
import Data.Maybe
import Data.List
import Syntax.GeometricUtils 
import SAT.Impl
import System.Console.Haskeline
import System.Environment
import Tools.Config
import Control.Monad.Trans

main :: IO ()
main = do
  -- init display
  displayInit
  -- get configuration
  config <- getConfig 
  -- print preprocess information
  putStrLn "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>"
  putStrLn "Input Options: "
  putStrLn $ show config
  putStrLn "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>"
  -- get the start state
  startState <- getStartState config
  case startState of
    Left (UErr err) -> error err
    Right state@(theory, prov, stream, model) -> do
      (prettyPrint 0 flow (show model))
      runInputT defaultSettings (loop state)
  -- exit display
  displayExit

loop :: UState -> InputT IO ()
loop state@(theory, prov, stream, model) = do
  -- possible REPL loops
  let sameLoop = loop state
  let newLoop state'@(theory', prov', stream', model') = (lift $ prettyPrint 0 flow (show model')) >> loop state'
  -- get input
  minput <- getInputLine "% "
  -- parse input into a command and act depending on the case
  case minput of
      Nothing -> return ()
      Just command -> case (parseCommand command) of
        -- display
        Display thing -> case thing of
          DispTheory -> (lift $ mapM_ (\s-> prettyPrint 0 finput ((show s)++"\n")) theory) >> sameLoop
          DispModel -> newLoop state
        -- exploration
        Go explore -> case explore of
          Next -> case getNextModel state of
            Left (UErr err) -> (lift $ prettyPrint 0 ferror (err++"\n")) >> sameLoop
            Right state' -> newLoop state'
          Augment term -> (lift $ prettyPrint 0 ferror "not implemented\n") >> sameLoop
        -- explanation
        Ask question -> case question of
          Name isall isrec term -> do
            let origins = getOrigin state (isall,isrec) term
            printOrigin theory (isall, 0) origins
            sameLoop
          Blame atom -> do
            let justification = getJustification state atom
            printJustification atom theory justification
            sameLoop
        -- others
        Other utility -> case utility of
          Help -> (lift $ prettyPrint 0 foutput helpCommand) >> sameLoop
          Exit -> (lift $ prettyPrint 0 foutput "closing...\n") >> return ()
        SyntaxError err -> (lift $ prettyPrint 0 ferror (err++"\n")) >> sameLoop

printOrigin :: Theory -> (Bool, Int) -> UOrigin -> InputT IO ()
printOrigin thy mods@(isall, tabs) (UOriginLeaf term origin) = do
  lift $ prettyPrint tabs foutput ("origin of "++(show term)++"\n")
  case origin of
    Left (UErr err) -> (lift $ prettyPrint tabs ferror (err++"\n"))
    Right namedthy -> printDiff (thy,namedthy) ((show term),tabs,isall)
printOrigin thy mods@(isall, tabs) (UOriginNode term origin depends) = do
  printOrigin thy mods (UOriginLeaf term origin)
  mapM_ (printOrigin thy (isall, tabs+1)) depends

printJustification :: Formula -> Theory -> Either UError UTheorySubs -> InputT IO()
printJustification atom theory justification = do 
  lift $ prettyPrint 0 foutput ("justification of "++(show atom)++"\n")
  case justification of
    Left (UErr err) -> (lift $ prettyPrint 0 ferror (err++"\n"))
    Right blamedthy -> printDiff (theory,blamedthy) ((show atom),0,True)

printDiff :: (Theory, UTheorySubs) -> (String, Int, Bool) -> InputT IO()
printDiff (theory,dtheory) format@(highlight,tabs,printall) = printDiffPlus (zip theory dtheory) format
printDiffPlus :: [(Sequent, Maybe Sequent)] -> (String, Int, Bool) -> InputT IO()
printDiffPlus [] _ = return ()
printDiffPlus diff format@(highlight,tabs,printall) = do
  let (s, ms) = head diff
  let keepprinting = printDiffPlus (tail diff) format
  case ms of
    Nothing -> keepprinting
    Just ds -> do
      lift $ prettyPrint tabs finput ("theory rule: "++(show s)++"\n")
      lift $ prettyHighlight tabs highlight ("instance: "++(show ds)++"\n")
      if printall
        then keepprinting
        else return ()