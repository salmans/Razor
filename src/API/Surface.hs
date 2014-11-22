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
import Syntax.GeometricUtils
import SAT.Impl
import Tools.Config
import Data.List
import Data.Either
import Data.Maybe
import qualified Data.Map as Map
import System.Environment

data UState = UState (Config, Theory) (ChasePossibleFactsType, ProvInfo, SATTheoryType, Int) (SATIteratorType, Model)
type UBlame = Either UError (Blame, Sequent)
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
          let (b,p,t,c) = generateGS config thy
          let stream = modelStream t
          case (nextModel stream) of
            (Nothing, _) -> return $ Left (UErr "no models exist for given theory")
            (Just mdl', stream') -> return $ Right (UState (config,thy) (b,p,t,c) (stream',mdl'))
        Nothing -> return $ Left (UErr "Unable to parse input theory!")

getAugmentedState :: UState -> Formula -> Either UError UState
getAugmentedState state@(UState (cfg, thy) (b,p,t,c) (stream, mdl)) fml = case getObservation fml of
  Nothing -> Left (UErr "augmentation formula is not an observation")
  Just obs -> do
    let (b', p', t', c') = augment cfg thy (b, p, t, c) obs
    case nextModel (modelStream t') of
      (Nothing, _) -> Left (UErr "no models exist for given augmentation")
      (Just mdl', stream') -> Right (UState (cfg,thy) (b',p',t',c') (stream',mdl'))

getNextModel :: UState -> Either UError UState
getNextModel state@(UState (cfg, thy) (b,p,t,c) (stream, mdl)) = case (nextModel stream) of
  (Nothing, stream') -> Left (UErr "no more minimal models")
  (Just mdl', stream') -> Right (UState (cfg,thy) (b,p,t,c) (stream',mdl'))

getOrigin :: UState -> Bool -> Term -> [UOrigin]
getOrigin state@(UState (cfg, thy) (b,p,t,_) (stream, mdl)) isrec term = do
  case name of
    Left err -> [UOriginLeaf term (Left err)]
    Right origins -> do
      (origin, nextelms) <- origins
      case isrec of
        False -> return $ UOriginLeaf term (blamed origin)
        True -> return $ UOriginNode term (blamed origin) (concatMap (\e->(getOrigin state isrec (Elem e))) nextelms)
  where 
    name = case getEqualElements mdl term of
      [] -> Left (UErr ("element "++(show term)++" not in the current model"))
      eqelms -> do
        case getElementBlames thy (elementProvs p) mdl eqelms of
          [] -> Left (UErr ("no provenance information for element "++(show term)++"\n"))
          origins -> Right origins
    blamed origin = case getBlamedSequent t origin of
      Nothing -> Left $ UErr $ "unable to find blamed theory sequent from provenance info\n"++(show origin)
      Just bseq -> Right (origin, bseq)
                
getJustification :: UState -> Formula -> UBlame
getJustification state@(UState (cfg, thy) (b,p,t,_) (stream, mdl)) fml = case getObservation fml of
  Nothing -> Left (UErr "blame formula is not an observation")
  Just obv -> case getObservationBlame (observationProvs p) mdl obv of
    Nothing -> Left (UErr "no provenance info for blame observation")
    Just blame -> case getBlamedSequent t blame of
      Nothing -> Left $ UErr $ "unable to find blamed theory sequent from provenance info"
      Just bseq -> Right (blame, bseq)
