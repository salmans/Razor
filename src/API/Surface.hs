{-|
  Razor
  Module      : API.Surface
  Description : The module pides a surface API for user level commands interacting with the core API
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module API.Surface where
import Chase.Impl
import API.Core
import Common.Model
import Common.Provenance
import SAT.IData
import Common.Observation
import Syntax.GeometricUtils
import SAT.Impl
import Tools.Config
import Data.List
import Data.Either
import Data.Maybe
import qualified Data.Map as Map
import System.Environment

data UState = UState (Config, Theory) (ChaseHerbrandBaseType, ProvInfo, SATTheoryType) (SATIteratorType, Model) ModelProv
type UBlame = Either UError (Blame, ObservationSequent)
data UOrigin = UOriginLeaf Term UBlame | UOriginNode Term UBlame [UOrigin]
data UError = UErr String
data UAnswer = AOrigin UOrigin | ABlame UBlame

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
          let (b,p,t) = generateGS config thy
          let stream = modelStream t
          case (nextModel stream) of
            (Nothing, _) -> return $ Left (UErr "no models exist for given theory")
            (Just mdl', stream') -> return $ Right (UState (config,thy) (b,p,t) (stream',mdl') (deriveModelProv thy p mdl'))
        Nothing -> return $ Left (UErr "Unable to parse input theory!")

getAugmentedState :: UState -> Formula -> Either UError UState
getAugmentedState state@(UState (cfg, thy) (b,p,t) (stream, mdl) modelProv) fml = case getObservation fml of
  Nothing -> Left (UErr "augmentation formula is not an observation")
  Just obs -> do
    let (b', p', t') = augment cfg thy (b, p, t) obs
    case nextModel (modelStream t') of
      (Nothing, _) -> Left (UErr "no models exist for given augmentation")
      (Just mdl', stream') -> Right (UState (cfg,thy) (b',p',t') (stream',mdl') (deriveModelProv thy p' mdl'))

getNextModel :: UState -> Either UError UState
getNextModel state@(UState (cfg, thy) (b,p,t) (stream, mdl) modelProv) = case (nextModel stream) of
  (Nothing, stream') -> Left (UErr "no more minimal models")
  (Just mdl', stream') -> Right (UState (cfg,thy) (b,p,t) (stream',mdl') (deriveModelProv thy p mdl'))

getOrigin :: UState -> (Bool, Bool) -> Term -> UOrigin
getOrigin state@(UState (cfg, thy) (b,p,t) (stream, mdl) modelProv) mods@(isall, isrec) term = do
  case name of
    Left err -> UOriginLeaf term (Left err)
    Right (origin, nextterms) -> do
      case isrec of
        False -> UOriginLeaf term (blamed origin)
        True -> UOriginNode term (blamed origin) (map (getOrigin state mods) nextterms)
  where 
    name = case getEqualElements mdl term of
      [] -> Left (UErr ("element "++(show term)++" not in the current model"))
      eqelms -> do
        case getElementBlameTree thy (elementProvs p) mdl eqelms of
          Nothing -> Left (UErr ("no provenance information for element "++(show term)++"\n"))
          Just (blame, nextelms) -> Right (blame, (map Elem nextelms))
    blamed origin = case getBlamedSequent t origin of
      Nothing -> Left $ UErr $ "unable to find blamed theory sequent from provenance info"
      Just bseq -> Right (origin, bseq)
                
getJustification :: UState -> Formula -> UBlame
getJustification state@(UState (cfg, thy) (b,p,t) (stream, mdl) modelProv) fml = case getObservation fml of
  Nothing -> Left (UErr "blame formula is not an observation")
  Just obv -> case getObservationBlame (observationProvs p) mdl obv of
    Nothing -> Left (UErr "no provenance info for blame observation")
    Just blame -> case getBlamedSequent t blame of
      Nothing -> Left $ UErr $ "unable to find blamed theory sequent from provenance info"
      Just bseq -> Right (blame, bseq)

replaceTheory :: Theory -> TheorySub -> [Maybe Sequent]
replaceTheory thy reps = do
  sequent <- thy
  case (elemIndex sequent thy) of
    Nothing -> return Nothing
    Just i -> case (Map.lookup i reps) of
      Nothing -> return Nothing
      Just rs -> do
        let (Sequent b h) = sequent
        let (Sequent b' h') = (Sequent b (replaceExists h 0 (existSub rs)))
        let (Sequent b'' h'') = (Sequent (replaceFrees b' (freeSub rs)) (replaceFrees h' (freeSub rs)))
        return $ Just (Sequent (replaceFuncs b'' (funcSub rs)) (replaceFuncs h'' (funcSub rs)))
