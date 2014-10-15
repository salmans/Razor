{-|
  Razor
  Module      : Main
  Description : The module provides a REPL for user interaction with Razor.
  Maintainer  : Salman Saghafi, Ryan Danas
-}
{-| TODO
  replace functions with elements and how to deal with flattening... exists x. exists y. Q(x, y, f(g(x, y)))
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
      (prettyPrint 0 flow (show model'))
      -- enter the repl
      runInputT defaultSettings (loop (model', stream') prov theory)
  -- exit display
  displayExit

loop :: (Model, SATIteratorType) -> ProvInfo -> Theory -> InputT IO ()
loop (model, stream) prov thy = do
  let sameLoop = loop (model, stream) prov thy
  let newLoop (model', stream') = (lift $ prettyPrint 0 flow (show model')) >> loop (model', stream') prov thy
  minput <- getInputLine "% "
  case minput of
      Nothing -> return ()
      Just command -> case (parseCommand command) of
        Display thing -> case thing of
          DispTheory -> (lift $ mapM_ (\s-> prettyPrint 0 finput ((show s)++"\n")) thy) >> sameLoop
          DispModel -> newLoop (model, stream)
        Go explore -> case explore of
          Next -> do
            case (nextModel stream) of
              (Nothing, stream') -> (lift $ prettyPrint 0 ferror "no more minimal models available\n") >> sameLoop
              (Just model', stream') -> newLoop (model', stream')
          Augment term -> (lift $ prettyPrint 0 ferror "not implemented\n") >> sameLoop
        Ask question -> case question of
          Name isall isrec term -> (origin thy prov model [term] (isall,isrec,0)) >> sameLoop
          Blame atom -> (justify thy prov model atom) >> sameLoop
        Other utility -> case utility of
          Help -> (lift $ prettyPrint 0 foutput helpCommand) >> sameLoop
          Exit -> (lift $ prettyPrint 0 foutput "closing...\n") >> return ()
        SyntaxError err -> (lift $ prettyPrint 0 ferror (err++"\n")) >> sameLoop


-- Naming
origin :: Theory -> ProvInfo -> Model -> [Term] -> (Bool, Bool, Int) -> InputT IO ()
origin _ _ _ [] (_,_,_)= return ()
origin thy prov mdl terms (isall,isrec,tabs) = do
  nextterms <- (mapM (\t-> (name thy prov mdl t (isall,tabs))) terms)
  if isrec
    then origin thy prov mdl (concat nextterms) (isall,isrec,(tabs+1))
    else return ()
name :: Theory -> ProvInfo -> Model -> Term -> (Bool,Int) -> InputT IO ([Term])
name thy prov mdl term (isall,tabs) = do
  case (getEqualElements mdl term) of
    [] -> (lift $ (prettyPrint tabs ferror ("element "++(show term)++" not in the current model\n"))) >> return []
    eqelms -> do
      case (getSkolemTrees prov mdl term) of
        [] -> (lift $ (prettyPrint tabs ferror ("no provenance information for element "++(show term)++"\n"))) >> return []
        skolemtrees -> do
          -- Look at the first or all skolem trees depending on user input
          case isall of
            True -> do
              let (actualelm,_,_) = head skolemtrees
              let allelms = map (\(e, h, r)->e) skolemtrees
              let skolemnext = concat (map (\(e, h, r)->r) skolemtrees)
              let names = (map (\(e, h, r)->(actualelm, h, r)) skolemtrees) 
              let namedthy = nameTheory thy names
              lift $ prettyPrint tabs foutput ("origin of "++(show actualelm)++"(equal to "++(show allelms)++")... depends on origin of "++(show skolemnext)++"\n")
              printDiff (thy,namedthy) ((show actualelm),tabs,isall)
              return (map (\e->(Elem e)) skolemnext)
            False -> do
              let (elm, skolemhead, skolemnext) = (head skolemtrees)
              let namedthy = (nameTheory thy [(elm, skolemhead, skolemnext)])
              lift $ prettyPrint tabs foutput ("origin of "++(show elm)++"... depends on origin of "++(show skolemnext)++"\n")
              printDiff (thy,namedthy) ((show elm),tabs,isall)
              return (map (\e->(Elem e)) skolemnext)

-- Blaming
justify :: Theory -> ProvInfo -> Model -> Formula -> InputT IO()
justify theory prov model atom = case (getFact model atom) of
  Nothing -> lift $ prettyPrint 0 ferror ("fact not in form FactName(e^0, e^1, ...) or is not in the current model\n") >> return ()
  Just fact@(factname, factelms) -> case (getBlame prov model fact) of
    [] -> lift $ prettyPrint 0 ferror ("no provenance information for fact "++(show atom)++"\n") >> return ()
    blames -> do
      let names = (concatMap (\t -> do
                                      let skolemtrees = (getSkolemTrees prov model t) 
                                      let (actualelm,_,_) = head skolemtrees
                                      map (\(e, h, r)->(actualelm, h, r)) skolemtrees) factelms)
      let blamedthy = (blameTheory theory names blames)
      lift $ prettyPrint 0 foutput ("justification of "++(show atom)++"\n")
      printDiff (theory,blamedthy) ((show atom),0,True)

-- Misc 
printDiff :: (Theory, [Maybe Sequent]) -> (String, Int, Bool) -> InputT IO()
printDiff (thy,dthy) format@(highlight,tabs,printall) = printDiffPlus (zip thy dthy) format
printDiffPlus :: [(Sequent, Maybe Sequent)] -> (String, Int, Bool) -> InputT IO()
printDiffPlus [] _ = return ()
printDiffPlus diff format@(highlight,tabs,printall) = do
  let (s, ms) = head diff
  let keepprinting = printDiffPlus (tail diff) format
  case ms of
    Nothing -> keepprinting
    Just ds -> do
      lift $ prettyPrint tabs finput ("thy rule: "++(show s)++"\n")
      lift $ prettyHighlight tabs highlight ("instance: "++(show ds)++"\n")
      if printall
        then keepprinting
        else return ()