{-|
  Razor
  Module      : Main
  Description : The module provides a REPL for user interaction with Razor.
  Maintainer  : Salman Saghafi, Ryan Danas
-}
{-| TODO
  show model / show theory commands
  origin should only parse elements, not all terms
  origin should tell user more specific errors (element not in model)

  blameTheory doesnt name elts included in blame
  getBlame does not work for equality permutations

  syntax errors should be more verbose (help options for command entered?)
  suppress 'Truth=>' in prettySequent
  ignore parens in user input
  refactor user commands / help into a data structure like command line options?
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
  putStrLn "Theory: "
  mapM_ (putStrLn.show) theory
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
        Nothing -> (lift $ prettyPrint 0 ferror "syntax error\n") >> sameLoop
        Just cmd -> case cmd of
          Go explore -> case explore of
            Next -> do
              case (nextModel stream) of
                (Nothing, stream') -> (lift $ prettyPrint 0 ferror "no more minimal models available\n") >> sameLoop
                (Just model', stream') -> newLoop (model', stream')
            Augment term -> (lift $ prettyPrint 0 ferror "not implemented\n") >> sameLoop
          Ask question -> case question of
            Name isrec term -> (origin thy prov [term] isrec 0) >> sameLoop
            Blame atom -> (justify thy prov model atom) >> sameLoop
          Other utility -> case utility of
            Help -> (lift $ prettyPrint 0 finfo helpCommand) >> sameLoop
            Exit -> (lift $ prettyPrint 0 finfo "closing...\n") >> return ()

-- Naming
origin :: Theory -> ProvInfo -> [Term] -> Bool -> Int -> InputT IO ()
origin _ _ [] _ _ = return ()
origin thy prov terms bfs tabs = do
  nextterms <- (mapM (\t-> (name thy prov t tabs)) terms)
  if bfs
    then (origin thy prov (concat nextterms) bfs (tabs+1))
    else return ()
name :: Theory -> ProvInfo -> Term -> Int -> InputT IO ([Term])
name thy prov term tabs = do
  case (getSkolemTree prov term) of
    Nothing -> (lift $ (prettyPrint tabs ferror ("no provenance information for "++(show term)++"\n"))) >> return []
    Just ((Element elm), skolemhead, skolemrest) -> do
      let skolemnext = (map (getSkolemElement prov) skolemrest)
      let namedthy = (nameTheory thy ((Element elm), skolemhead, skolemnext))
      lift $ prettyPrint tabs fhighc ("Origin of "++(show (Element elm))++"... depends on origin of "++(show skolemnext)++"\n")
      printDiff thy namedthy elm tabs
      return (map (\e->(Elem e)) skolemnext)

-- Blaming
justify :: Theory -> ProvInfo -> Model -> Formula -> InputT IO()
justify theory prov model atom = case (getBlame prov model atom) of
  (_, []) -> (lift $ prettyPrint 0 ferror ("no provenance information for "++(show atom)++"\n")) >> return ()
  (terms, blames) -> do
    lift $ prettyPrint 0 fhighc ("Justification of "++(show atom)++"\n")
    printDiff theory (blameTheory prov theory terms blames) (show atom) 0

-- Misc 
printDiff :: Theory -> Theory -> String -> Int -> InputT IO()
printDiff thy dthy diffhigh tabs = lift $ mapM_ (\(sequent, dsequent)
  -> if (sequent==dsequent)
    then return ()
    else do
      prettyPrint tabs finfo ("Thy Rule: "++(show sequent)++"\n")
      prettyHighlight tabs diffhigh ("Instance: "++(show dsequent)++"\n")) (zip thy dthy)