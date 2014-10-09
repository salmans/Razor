{-|
  Razor
  Module      : Main
  Description : The module provides a REPL for user interaction with Razor.
  Maintainer  : Salman Saghafi, Ryan Danas
-}
{-| TODO
  augmentation
  squash remaining bugs
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
    (Nothing, stream') -> (prettyPrint ferror "no models found\n")
    (Just model', stream') -> do
      (prettyPrint finfo (show model'))
      -- enter the repl
      runInputT defaultSettings (loop (model', stream') prov theory)
  -- exit display
  displayExit

loop :: (Model, SATIteratorType) -> ProvInfo -> Theory -> InputT IO ()
loop (model, stream) prov thy = do
  let sameLoop = loop (model, stream) prov thy
  let newLoop (model', stream') = (lift $ prettyPrint finfo (show model')) >> loop (model', stream') prov thy
  minput <- getInputLine "% "
  case minput of
      Nothing -> return ()
      Just command -> case (parseCommand command) of
        Nothing -> (lift $ prettyPrint ferror "syntax error\n") >> sameLoop
        Just cmd -> case cmd of
          Go explore -> case explore of
            Next -> do
              case (nextModel stream) of
                (Nothing, stream') -> (lift $ prettyPrint ferror "no more minimal models available\n") >> sameLoop
                (Just model', stream') -> newLoop (model', stream')
            Augment term -> do
              (lift $ prettyPrint ferror "not implemented\n")
              sameLoop 
          Ask question -> case question of
            Name isrec term -> do
              (origin thy prov [term] isrec)
              sameLoop
            -- blame
            Blame atom -> do
              case (getBlame prov model atom) of
                [] -> (lift $ prettyPrint ferror ("no provenance information for "++(show atom)++"\n")) >> sameLoop
                blames -> do
                  let blamedthy = (blameTheory thy blames)
                  lift $ mapM_ (\(sequent, blamedsequent) 
                      -> case blamedsequent of
                        Just blamed -> do
                          prettyPrint finfo ((show (fromJust sequent))++"\n")
                          prettyHighlight (show atom) ((show blamed)++"\n")
                        Nothing -> return ()) (zip (map Just thy) blamedthy)
                  sameLoop
          Other utility -> case utility of
            Help -> do
              (lift $ prettyPrint finfo helpCommand)
              sameLoop
            Exit -> do
              (lift $ prettyPrint finfo "exiting...\n")
              return ()

origin :: Theory -> ProvInfo -> [Term] -> Bool -> InputT IO ()
origin _ _ [] _ = return ()
origin thy prov terms bfs = do
  nextterms <- (mapM (\t-> (name thy prov t)) terms)
  if bfs
    then (origin thy prov (concat nextterms) bfs)
    else return ()

name :: Theory -> ProvInfo -> Term -> InputT IO ([Term])
name thy prov term = do
  case (getSkolemTree prov term) of
    Nothing -> do
      (lift $ (prettyPrint ferror ("no provenance information for " ++ (show term) ++ "\n")))
      return []
    Just ((Element elm), skolemhead, skolemrest) -> do
      let skolemnext = (map (getSkolemElement prov) skolemrest)
      let namedthy = (nameTheory (Element elm) skolemhead skolemnext thy)
      lift $ prettyPrint fhighc ((show elm)++(show skolemnext)++"\n")
      lift $ mapM_ (\(sequent, namedsequent) 
        -> if (sequent==namedsequent)
          then return ()
          else do
            prettyPrint finfo ((show sequent)++"\n")
            prettyHighlight elm ((show namedsequent)++"\n")) (zip thy namedthy)
      return (map (\e->(Elem e)) skolemnext)
