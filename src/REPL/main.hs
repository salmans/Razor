{-|
  Razor
  Module      : Main
  Description : The module provides a REPL for user interaction with Razor.
  Maintainer  : Salman Saghafi, Ryan Danas
-}
{-| TODO
  augmentation
-}

module Main where
import API
import Common.Model
import Common.Provenance
import Data.Maybe
import Data.List
import REPL.Syntax
import REPL.Ansi.Display
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
  args <- getArgs
  config <- API.parseConfig args
  -- read in user input file
  let inputFileName = if   (isJust.configInput) config
                      then (fromJust.configInput) config
                      else error "No input file is specified!"
  input <- readFile inputFileName
  -- parse it into a theory
  thy <- API.parseTheory config input
  theory <- case thy of
    Just thy -> return thy
    Nothing -> error "Unable to parse input theory!"
  -- print preprocess information
  putStrLn $ "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>"
  putStrLn "Input Options: "
  putStrLn (show config)
  putStrLn $ "<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>"
  -- generate G*, get the model stream
  let (base, prov, prop) = generateGS config theory
  let stream = modelStream prop 
  -- get the first model
  case (nextModel stream) of
    (Nothing, stream') -> (prettyPrint 0 ferror "no models found\n")
    (Just model', stream') -> do
      (prettyPrint 0 finfo (show model'))
      -- enter the repl
      runInputT defaultSettings (loop (model', stream') prov theory)
  -- exit display
  displayExit

loop :: (Model, SATIteratorType) -> ProvInfo -> Theory -> InputT IO ()
loop (model, stream) prov thy = do
  let sameLoop = loop (model, stream) prov thy
  let newLoop (model', stream') = (lift $ prettyPrint 0 finfo (show model')) >> loop (model', stream') prov thy
  minput <- getInputLine "% "
  case minput of
      Nothing -> return ()
      Just command -> case (parseCommand command) of
        Display thing -> case thing of
          DispTheory -> (lift $ mapM_ (\s-> prettyPrint 0 finfo ((show s)++"\n")) thy) >> sameLoop
          DispModel -> newLoop (model, stream)
        Go explore -> case explore of
          Next -> do
            case (nextModel stream) of
              (Nothing, stream') -> (lift $ prettyPrint 0 ferror "no more minimal models available\n") >> sameLoop
              (Just model', stream') -> newLoop (model', stream')
          Augment term -> (lift $ prettyPrint 0 ferror "not implemented\n") >> sameLoop
        Ask question -> case question of
          Name isrec term -> (origin thy prov model [term] isrec 0) >> sameLoop
          Blame atom -> (justify thy prov model atom) >> sameLoop
        Other utility -> case utility of
          Help -> (lift $ prettyPrint 0 finfo helpCommand) >> sameLoop
          Exit -> (lift $ prettyPrint 0 finfo "closing...\n") >> return ()
        SyntaxError err -> (lift $ prettyPrint 0 ferror (err++"\n")) >> sameLoop


-- Naming
origin :: Theory -> ProvInfo -> Model -> [Term] -> Bool -> Int -> InputT IO ()
origin _ _ _ [] _ _ = return ()
origin thy prov mdl terms bfs tabs = do
  nextterms <- (mapM (\t-> (name thy prov mdl t tabs)) terms)
  if bfs
    then (origin thy prov mdl (concat nextterms) bfs (tabs+1))
    else return ()
name :: Theory -> ProvInfo -> Model -> Term -> Int -> InputT IO ([Term])
name thy prov mdl term tabs = do
  case (getEqualElements mdl term) of
    [] -> (lift $ (prettyPrint tabs ferror ("element "++(show term)++" not in the current model\n"))) >> return []
    eqelms -> do
      case (getSkolemTrees prov mdl term) of
        [] -> (lift $ (prettyPrint tabs ferror ("no provenance information for element "++(show term)++"\n"))) >> return []
        skolemtrees -> do
          -- TODO For now, just show one origin, not all
          let (elm, skolemhead, skolemnext) = (head skolemtrees)
          let namedthy = (nameTheory thy [(elm, skolemhead, skolemnext)])
          lift $ prettyPrint tabs fhighc ("origin of "++(show elm)++"... depends on origin of "++(show skolemnext)++"\n")
          -- TODO For now, just show one origin
          printDiff (thy,namedthy) ((show elm),tabs,False)
          return (map (\e->(Elem e)) skolemnext)

-- Blaming
justify :: Theory -> ProvInfo -> Model -> Formula -> InputT IO()
justify theory prov model atom = case (getFact model atom) of
  Nothing -> lift $ prettyPrint 0 ferror ("fact not in form FactName(e^0, e^1, ...) or is not in the current model\n") >> return ()
  Just fact@(factname, factelms) -> case (getBlame prov model fact) of
    [] -> lift $ prettyPrint 0 ferror ("no provenance information for fact "++(show atom)++"\n") >> return ()
    blames -> do
      let names = (concatMap (\t->(getSkolemTrees prov model t)) factelms)
      let blamedthy = (blameTheory theory names blames)
      lift $ prettyPrint 0 fhighc ("justification of "++(show atom)++"\n")
      -- TODO For now, just show one blame
      printDiff (theory,blamedthy) ((show atom),0,False)

-- Misc 
printDiff :: (Theory, [Maybe Sequent]) -> (String, Int, Bool) -> InputT IO()
printDiff (thy,dthy) format@(highlight,tabs,printall) = printDiffPlus (zip thy dthy) format
printDiffPlus :: [(Sequent, Maybe Sequent)] -> (String, Int, Bool) -> InputT IO()
printDiffPlus diff format@(highlight,tabs,printall) = do
  let (s, ms) = head diff
  let keepprinting = printDiffPlus (tail diff) format
  case ms of
    Nothing -> keepprinting
    Just ds -> do
      lift $ prettyPrint tabs finfo ("thy rule: "++(show s)++"\n")
      lift $ prettyHighlight tabs highlight ("instance: "++(show ds)++"\n")
      if printall
        then keepprinting
        else return ()