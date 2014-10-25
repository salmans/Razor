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
type UState = (Theory, ProvInfo, SATIteratorType, Model, ModelProv)
type UTheorySubs = [Maybe Sequent]
data UOrigin = UOriginLeaf Term (Either UError UTheorySubs) | UOriginNode Term (Either UError UTheorySubs) [UOrigin]

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
      thy <- parseTheory config input
      case thy of
        Just thy -> do
          let (base, prov, prop) = generateGS config thy
          let stream = modelStream prop
          case (nextModel stream) of
            (Nothing, stream') -> return $ Left (UErr "no models available")
            (Just mdl', stream') -> return $ Right (thy, prov, stream', mdl', (deriveModelProv prov mdl'))
        Nothing -> return $ Left (UErr "Unable to parse input theory!")

getNextModel :: UState -> Either UError UState
getNextModel state@(thy, prov, stream, mdl, modelProv) = case (nextModel stream) of
  (Nothing, stream') -> Left (UErr "no more minimal models")
  (Just mdl', stream') -> Right (thy, prov, stream', mdl', (deriveModelProv prov mdl'))

getOrigin :: UState -> (Bool, Bool) -> Term -> UOrigin
getOrigin state@(thy, prov, stream, mdl, modelProv) mods@(isall, isrec) term = do
  case name of
    Left err -> UOriginLeaf term (Left err)
    Right (namedtheory, nextterms) -> do
      case isrec of
        False -> UOriginLeaf term (Right namedtheory)
        True -> UOriginNode term (Right namedtheory) (map (getOrigin state mods) nextterms)
  where 
    name = case (getEqualElements mdl term) of
      [] -> Left (UErr ("element "++(show term)++" not in the current model"))
      eqelms -> do
        case (getSkolemTrees prov mdl term) of
          [] -> Left (UErr ("no provenance information for element "++(show term)++"\n"))
          skolemtrees -> do
            case isall of
              True -> do
                let (actualelm,_,_) = head skolemtrees
                let allelms = map (\(e, h, r)->e) skolemtrees
                let skolemnext = concat (map (\(e, h, r)->r) skolemtrees)
                let names = (map (\(e, h, r)->(actualelm, h, r)) skolemtrees) 
                let namedtheory = nameTheory thy names
                let nextterms = (map (\e->(Elem e)) skolemnext)
                return (namedtheory, nextterms)
              False -> do
                let (elm, skolemhead, skolemnext) = (head skolemtrees)
                let namedtheory = (nameTheory thy [(elm, skolemhead, skolemnext)])
                let nextterms = (map (\e->(Elem e)) skolemnext)
                return (namedtheory, nextterms)
                
getJustification :: UState -> Formula -> Either UError UTheorySubs
getJustification state@(thy, prov, stream, mdl, modelProv) atom = case (getFact mdl atom) of
  Nothing -> Left (UErr "fact not in form FactName(e^0, e^1, ...) or is not in the current model")
  Just fact@(factname, factelms) -> case (getBlame prov mdl fact) of
    [] -> Left (UErr ("no provenance information for fact "++(show atom)))
    blames -> do
      let names = (concatMap (\t -> do
                                      let skolemtrees = (getSkolemTrees prov mdl t) 
                                      let (actualelm,_,_) = head skolemtrees
                                      map (\(e, h, r)->(actualelm, h, r)) skolemtrees) factelms)
      let blamedtheory = (blameTheory thy names blames)
      Right blamedtheory
