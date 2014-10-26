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
import Data.List
import Data.Either
import Data.Maybe
import qualified Data.Map as Map
import System.Environment

data UError = UErr String
type UState = (Theory, ProvInfo, SATIteratorType, Model, ModelProv)
data UOrigin = UOriginLeaf Term (Either UError TheorySub) | UOriginNode Term (Either UError TheorySub) [UOrigin]
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
      thy <- parseTheory config input
      case thy of
        Just thy -> do
          let (base, prov, prop) = generateGS config thy
          let stream = modelStream prop
          case (nextModel stream) of
            (Nothing, stream') -> return $ Left (UErr "no models available")
            (Just mdl', stream') -> return $ Right (thy, prov, stream', mdl', (deriveModelProv thy prov mdl'))
        Nothing -> return $ Left (UErr "Unable to parse input theory!")

getNextModel :: UState -> Either UError UState
getNextModel state@(thy, prov, stream, mdl, modelProv) = case (nextModel stream) of
  (Nothing, stream') -> Left (UErr "no more minimal models")
  (Just mdl', stream') -> Right (thy, prov, stream', mdl', (deriveModelProv thy prov mdl'))

getOrigin :: UState -> (Bool, Bool) -> Term -> UOrigin
getOrigin state@(thy, prov, stream, mdl, modelProv) mods@(isall, isrec) term = do
  case name of
    Left err -> UOriginLeaf term (Left err)
    Right (thynames, nextterms) -> do
      case isrec of
        False -> UOriginLeaf term (Right thynames)
        True -> UOriginNode term (Right thynames) (map (getOrigin state mods) nextterms)
  where 
    name = case (getEqualElements mdl term) of
      [] -> Left (UErr ("element "++(show term)++" not in the current model"))
      eqelms -> do
        case (Map.lookup (head eqelms) (nameProv modelProv)) of
          Nothing -> Left (UErr ("no provenance information for element "++(show term)++"\n"))
          Just (thynames, nextterms) -> Right (thynames, (map Elem nextterms))
                
getJustification :: UState -> Formula -> Either UError UTheorySubs
getJustification state@(thy, prov, stream, mdl, modelProv) atom = case (getFact mdl atom) of
  Nothing -> Left (UErr "fact not in form FactName(e^0, e^1, ...) or is not in the current model")
  Just fact@(factname, factelms) -> case (getBlame prov mdl fact) of
    [] -> Left (UErr ("no provenance information for fact "++(show atom)))
    blames -> do
      let names = (concatMap (\t -> do
                                      let skolemtrees = (getSkolemTrees (elementProvs prov) mdl t) 
                                      let (actualelm,_,_) = head skolemtrees
                                      map (\(e, h, r)->(actualelm, h, r)) skolemtrees) factelms)
      let blamedtheory = (blameTheory thy names blames)
      Right blamedtheory

--
--
replaceTheory :: Theory -> TheorySub -> [Maybe Sequent]
replaceTheory thy reps = do
  sequent <- thy
  case (elemIndex sequent thy) of
    Nothing -> return Nothing
    Just i -> case (Map.lookup i reps) of
      Nothing -> return Nothing
      Just rs -> do
        let (Sequent b h) = sequent
        let (Sequent b' h') = (Sequent b (replaceExists h (existSub rs)))
        let (Sequent b'' h'') = (Sequent (replaceFrees b' (freeSub rs)) (replaceFrees h' (freeSub rs)))
        return $ Just (Sequent (replaceFuncs b'' (funcSub rs)) (replaceFuncs h'' (funcSub rs)))
        