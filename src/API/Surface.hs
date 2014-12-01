{-|
  Razor
  Module      : API.Surface
  Description : The module pides a surface API for user level commands interacting with the core API
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module API.Surface where
import Chase.Impl
import API.Core
import Common.Input (Input (..))
import Common.Model
import Common.Provenance
import SAT.IData
import Syntax.GeometricUtils
import SAT.Impl
import Control.Exception
import Tools.Config
import Data.List
import Data.Either
import Data.Maybe
import qualified Data.Map as Map
import System.Environment

data UState = UState (Config, Theory) (ChasePossibleFactsType, ProvInfo, SATTheoryType, Int) (SATIteratorType, Model)
data UError = UErr String
type UBlame = Either UError (Blame, Sequent)
data UOrigin = UOriginLeaf Term UBlame | UOriginNode Term UBlame [UOrigin]

-----------------
-- Razor State --
-----------------
data RazorState = RazorState Config (Maybe Theory) (Maybe GStar) (Maybe SATIteratorType) (Maybe Model)
type GStar = (ChasePossibleFactsType, ProvInfo, SATTheoryType, Int)
type Error = String

setupState :: IO RazorState
setupState = do 
  args <- getArgs
  config <- parseConfig args
  return $ RazorState config Nothing Nothing Nothing Nothing

teardownState :: RazorState -> ()
teardownState state@(RazorState config theory gstar stream model) = case stream of
  Nothing -> ()
  Just openstream -> closeStream openstream

----------------------------
-- Surface API Operations --
----------------------------
loadTheory :: Config -> String -> IO(Either Error (Theory, GStar))
loadTheory config file = do
  res1 <- try (readFile file) :: IO (Either SomeException String)
  case res1 of
    Left ex -> return $ Left $ "Unable to read file! "++(show ex)
    Right raw -> do
      res2 <- try (parseInputFile config raw) :: IO (Either SomeException (Maybe Input)) 
      case res2 of
        Left ex -> return $ Left $ "Unable to parse file! "++(show ex)
        Right input -> case input of
          Just (Input thy dps) -> do
            let gs = generateGS config {configSkolemDepth = dps} thy
            return $ Right (thy, gs)
          Nothing -> return $ Left $ "Unable to parse input theory!"

modelNext :: Either (Config, SATTheoryType) SATIteratorType -> Maybe (SATIteratorType, Model)
modelNext seed = case seed of
  Left (config, t) -> case nextModel (openStream config t) of
    (Nothing, _) -> Nothing
    (Just mdl', stream') -> Just (stream',mdl')
  Right stream -> case nextModel stream of
    (Nothing, _) -> Nothing
    (Just mdl', stream') -> Just (stream',mdl')





modelUp :: UState -> Formula -> Either UError UState
modelUp state@(UState (cfg, thy) (b,p,t,c) (stream, mdl)) fml = case getObservation fml of
  Nothing -> Left (UErr "augmentation formula is not an observation")
  Just obs -> do
    let (b', p', t', c') = augment cfg thy (b, p, t, c) obs
    case nextModel (openStream cfg t') of
      (Nothing, _) -> Left (UErr "no models exist for given augmentation")
      (Just mdl', stream') -> Right (UState (cfg,thy) (b',p',t',c') (stream',mdl'))



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
