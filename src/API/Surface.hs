{-|
  Razor
  Module      : API.Surface
  Description : The module provides a surface API for user level commands interacting with the core API
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module API.Surface where
import API.Core
import Common.Model
import Common.Provenance
import Syntax.GeometricUtils
import SAT.Impl
import Tools.Config
import Data.Either
import Data.Maybe
import System.Environment

data UError = UErr String
type UState = (Theory, ProvInfo, SATIteratorType, Model)
type UTheorySubs = [Maybe Sequent]

getConfig :: IO Config
getConfig = do 
	args <- getArgs
	parseConfig args
getStartState :: Config -> IO (Either UError UState)
getStartState config = do
  case (configInput config) of
    Nothing -> return $ Left (UErr "No input file specified")
    Just infile -> do
      input <- readFile infile
      theory <- parseTheory config input
      case theory of
        Just thy -> do
          let (base, prov, prop) = generateGS config thy
          let stream = modelStream prop
          case (nextModel stream) of
            (Nothing, stream') -> return $ Left (UErr "no minimal models available")
            (Just model', stream') -> return $ Right (thy, prov, stream', model')
        Nothing -> return $ Left (UErr "Unable to parse input theory!")

getNextModel :: UState -> Either UError UState
getNextModel state@(thy, prov, stream, model) = case (nextModel stream) of
  (Nothing, stream') -> Left (UErr "no more minimal models")
  (Just model', stream') -> Right (thy, prov, stream', model')

getOrigin :: UState -> (Bool, Bool, Term) -> [(Term, Either UError UTheorySubs)]
getOrigin state@(theory, prov, stream, model) (isall, isrec, term) = origin (theory, prov, model) (isall, isrec) [term] 
origin :: (Theory, ProvInfo, Model) -> (Bool, Bool) -> [Term] -> [(Term, Either UError UTheorySubs)]
origin _ _ [] = []
origin substate (isall,isrec) terms = do
  term <- terms
  case name substate isall term of
    Left err -> return (term, Left err)
    Right (namedtheory, nextterms) -> do
      let termorigin = (term, Right namedtheory)
      case isrec of
        False -> return termorigin
        True -> concat $ return $ termorigin:(origin substate (isall,isrec) (concat (return nextterms)))
name :: (Theory, ProvInfo, Model) -> Bool -> Term -> Either UError (UTheorySubs, [Term])
name (theory, prov, model) isall term = do
  case (getEqualElements model term) of
    [] -> Left (UErr ("element "++(show term)++" not in the current model"))
    eqelms -> do
      case (getSkolemTrees prov model term) of
        [] -> Left (UErr ("no provenance information for element "++(show term)++"\n"))
        skolemtrees -> do
          -- Look at the first or all skolem trees depending on user input
          case isall of
            True -> do
              let (actualelm,_,_) = head skolemtrees
              let allelms = map (\(e, h, r)->e) skolemtrees
              let skolemnext = concat (map (\(e, h, r)->r) skolemtrees)
              let names = (map (\(e, h, r)->(actualelm, h, r)) skolemtrees) 
              let namedtheory = nameTheory theory names
              let nextterms = (map (\e->(Elem e)) skolemnext)
              return (namedtheory, nextterms)
            False -> do
              let (elm, skolemhead, skolemnext) = (head skolemtrees)
              let namedtheory = (nameTheory theory [(elm, skolemhead, skolemnext)])
              let nextterms = (map (\e->(Elem e)) skolemnext)
              return (namedtheory, nextterms)

