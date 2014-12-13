{- This file is part of Razor.

  Razor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Razor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Razor.  If not, see <http://www.gnu.org/licenses/>.

  Module      : API.Surface
  Description : The module pides a surface API for user level commands 
  interacting with the core API
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}
module API.Surface where
import Chase.Data (SequentMap)
import Chase.Impl
import API.Core
import Common.Input (Input (..))
import Common.Model
import Common.Provenance
import Common.Observation
import Common.Data (toSequent)
import Control.Applicative hiding ((<|>), many)
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

-----------------
-- Razor State --
-----------------
data RazorState = RazorState Config (Maybe Theory) ModelSpace (Maybe ModelCoordinate)
type ChaseState = (ChasePossibleFactsType, ProvInfo, SATIteratorType, Int)
type ModelSpace = Map.Map ModelCoordinate (ChaseState, Model)
data ModelCoordinate = Stream ModelCoordinate | Stack Observation ModelCoordinate | Origin
  deriving (Eq, Ord)
type Error = String

setupState :: IO RazorState
setupState = do 
  args <- getArgs
  config <- parseConfig args
  return $ RazorState config Nothing Map.empty Nothing

teardownState :: RazorState -> ()
teardownState state@(RazorState config theory mspace mcoor) = case mcoor of
  Nothing -> ()
  Just index -> case Map.lookup index mspace of
    Nothing -> ()
    Just ((b, p, opensat, c), _) -> closeSAT opensat

----------------------------
-- Surface API Operations --
----------------------------
loadTheory :: Config -> IO(Either Error (Config, Theory))
loadTheory config = case configInput config of
  Nothing -> return $ Left $ "No theory file specified!"
  Just file -> do
    res1 <- try (readFile file) :: IO (Either SomeException String)
    case res1 of
      Left ex -> return $ Left $ "Unable to read file! "++(show ex)
      Right raw -> do
        case parseInputFile config raw of
          Left err -> return $ Left $ "Unable to parse file! "++err
          Right (Input thy dps) -> do
            let config' = config {configSkolemDepth = dps}
            return $ Right (config', thy)

chaseTheory :: Config -> Theory -> ChaseState
chaseTheory config theory = generateChase config theory

modelspaceLookup :: ModelSpace -> ModelCoordinate -> Maybe (ChaseState, Model)
modelspaceLookup mspace mcoor = Map.lookup mcoor mspace

modelLookup :: ModelSpace -> Maybe ModelCoordinate -> Maybe Model
modelLookup mspace mcoor = case mcoor of
  Nothing -> Nothing
  Just mcoor' -> case Map.lookup mcoor' mspace of
    Nothing -> Nothing
    Just (_, model) -> Just model

modelNext :: Either ChaseState ModelCoordinate -> ModelSpace -> Maybe (ModelSpace, ModelCoordinate)
modelNext seed mspace = case seed of
  Left chasestate -> nextModelspace chasestate Origin mspace
  Right mcoor -> case modelspaceLookup mspace mcoor of
    Nothing -> Nothing
    Just (chasestate, _) -> nextModelspace chasestate mcoor mspace
  where
    nextModelspace (b, p, stream, c) mcoor mspace = case nextModel stream of
      (Nothing, _) -> Nothing
      (Just mdl', stream') -> do
        let mcoor' = Stream mcoor
        let mspace' = Map.insert mcoor' ((b, p, stream', c), mdl') mspace
        Just (mspace', mcoor')

modelUp :: Config -> Theory -> (Observation, [Element]) -> ModelSpace -> ModelCoordinate -> Maybe (ModelSpace, ModelCoordinate)
modelUp config theory (obs, newelms) mspace mcoor = case modelspaceLookup mspace mcoor of
  Nothing -> Nothing
  Just (chasestate, _) -> do
    let chasestate'@(b,p,stack,c) = augmentChase config theory chasestate (obs, newelms)
    case upModel stack of
      (Nothing, _) -> Nothing
      (Just mdl', stack') -> do
        let mcoor' = Stack obs mcoor
        let mspace' = Map.insert mcoor' ((b, p, stack', c), mdl') mspace
        Just (mspace', mcoor')

modelDown :: ModelSpace -> ModelCoordinate -> ModelCoordinate -> Maybe ModelSpace
modelDown mspace mcoor mcoor' = case (modelspaceLookup mspace mcoor, modelspaceLookup mspace mcoor') of
  (Just ((b,p,stack,c), _), Just ((b',p',_,c'), _)) -> do
    case downModel stack of
      (Nothing, _) -> Nothing
      (Just mdl', stack') -> do
        let mspace' = Map.insert mcoor' ((b', p', stack', c'), mdl') mspace
        Just mspace'
    
type QBlame = Either Error (Blame, Sequent)
--
--    
getJustification :: ChaseState -> Model -> Formula -> QBlame
getJustification gstar@(b,p,it,c) mdl fml = case getObservation mdl fml of
  Nothing -> Left "blame formula is not an observation"
  Just obv -> case getObservationBlame (observationProvs p) mdl obv of
    Nothing -> Left "no provenance info for blame observation"
    Just blame -> case findBlameSequent blame p of
      Nothing -> Left "unable to find blamed theory sequent from provenance info"
      Just bseq -> do
        let originalelms = formulaElements fml
        let eqelmss = map (getEqualElements mdl) (map Elem originalelms)
        Right (blame, foldr sequentRename (toSequent bseq) eqelmss)

data QOrigin = QOriginLeaf Term QBlame | QOriginNode Term QBlame [QOrigin]
--
--
getOrigin :: Theory -> ChaseState -> Model -> Bool -> Term -> [QOrigin]
getOrigin thy gstar@(b,p,it,c) mdl isrec term = do
  case name of
    Left err -> [QOriginLeaf term (Left err)]
    Right (eqelms, origins) -> do
      (origin, nextelms) <- origins
      case isrec of
        False -> return $ QOriginLeaf term (blamed origin eqelms)
        True -> case blamed origin eqelms of
          blame@(Right (_, (Sequent bd hd))) -> do
            let freeelms = formulaElements bd
            let childelms = nub (nextelms++freeelms)
            return $ QOriginNode term blame (concatMap (\e->(getOrigin thy gstar mdl isrec (Elem e))) childelms)
          blame -> return $ QOriginNode term blame (concatMap (\e->(getOrigin thy gstar mdl isrec (Elem e))) nextelms)
  where 
    name = case getEqualElements mdl term of
      [] -> Left $ "element "++(show term)++" not in the current model"
      eqelms -> do
        case getElementBlames thy (elementProvs p) mdl eqelms of
          [] -> Left $ "no provenance information for element "++(show term)
          origins -> Right (eqelms, origins)
    blamed origin eqelms = case findBlameSequent origin p of
      Nothing -> Left $ "unable to find blamed theory sequent from provenance info\n"++(show origin)
      Just bseq -> Right (origin, sequentRename eqelms (toSequent bseq))
--
-- an observation (for now) is just an atom consisting of only elements currently in the model
getObservation :: Model -> Formula -> Maybe Observation
getObservation model (Atm atm@(Rel rsym terms)) = do
  let elms = concat (map (\t->maybeToList (termToElement t)) terms)
  if (any null (map (getEqualElements model) terms)) || (length terms) /= (length elms)
    then Nothing
    else case toObservation atm of
      Just obv@(Obs (Rel rsym terms)) -> Just obv
      _ -> Nothing
getObservation _ _ = Nothing
--
-- 
getAugmentation :: Model -> Formula -> Maybe (Observation,[Element])
getAugmentation model (Atm atm@(Rel rsym terms)) = do
  let elms = concat (map (\t->maybeToList (termToElement t)) terms)
  if (length terms) /= (length elms)
    then Nothing
    else case toObservation atm of
      Just obv@(Obs (Rel rsym terms)) -> do
        let newelms = filter (\e->(null (getEqualElements model (Elem e)))) elms
        Just (obv,newelms)
      _ -> Nothing
getAugmentation _ _ = Nothing
