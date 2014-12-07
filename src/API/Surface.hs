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
import Chase.Impl
import API.Core
import Common.Input (Input (..))
import Common.Model
import Common.Provenance
import Common.Observation
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
data RazorState = RazorState Config (Maybe Theory) (Maybe ChaseState) ModelSpace (Maybe ModelCoordinate)
type ChaseState = (ChasePossibleFactsType, ProvInfo, SATTheoryType, Int)
type ModelSpace = Map.Map ModelCoordinate (SATIteratorType, Model)
data ModelCoordinate = Stream ModelCoordinate | Stack Observation ModelCoordinate | Origin
  deriving (Eq, Ord)
type Error = String

setupState :: IO RazorState
setupState = do 
  args <- getArgs
  config <- parseConfig args
  return $ RazorState config Nothing Nothing Map.empty Nothing

teardownState :: RazorState -> ()
teardownState state@(RazorState config theory gstar mspace mcoor) = case mcoor of
  Nothing -> ()
  Just index -> case Map.lookup index mspace of
    Nothing -> ()
    Just (opensat, _) -> closeSAT opensat

----------------------------
-- Surface API Operations --
----------------------------
loadTheory :: Config -> String -> IO(Either Error (Theory, ChaseState))
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
            let gs = generateChase config {configSkolemDepth = dps} thy
            return $ Right (thy, gs)
          Nothing -> return $ Left $ "Unable to parse input theory!"

modelspaceLookup :: ModelSpace -> ModelCoordinate -> Maybe (SATIteratorType, Model)
modelspaceLookup mspace mcoor = case Map.lookup mcoor mspace of
  Nothing -> Nothing
  Just (sat, model) -> Just (sat, model)

modelLookup :: ModelSpace -> Maybe ModelCoordinate -> Maybe Model
modelLookup mspace mcoor = case mcoor of
  Nothing -> Nothing
  Just mcoor' -> case Map.lookup mcoor' mspace of
    Nothing -> Nothing
    Just (sat, model) -> Just model

modelNext :: Either (Config, SATTheoryType) (ModelSpace, ModelCoordinate) -> Maybe (ModelSpace, ModelCoordinate)
modelNext seed = case seed of
  Left (config, t) -> case nextModel (openSAT config t) of
    (Nothing, _) -> Nothing
    (Just mdl', stream') -> do
      let mcoor' = Stream Origin
      let mspace' = Map.insert mcoor' (stream', mdl') Map.empty
      Just (mspace', mcoor')
  Right (mspace, mcoor) -> case Map.lookup mcoor mspace of
    Nothing -> Nothing
    Just (stream, mdl) -> case nextModel stream of
      (Nothing, _) -> Nothing
      (Just mdl', stream') -> do
        let mcoor' = Stream mcoor
        let mspace' = Map.insert mcoor' (stream', mdl') mspace
        Just (mspace', mcoor')

modelPrev :: SATIteratorType -> Maybe (SATIteratorType, Model)
modelPrev sat = Nothing

augment :: Config -> Theory -> ChaseState -> Formula -> Maybe ChaseState
augment config theory gstar@(b,p,t,c) fml = case getObservation fml of
  Nothing -> Nothing
  Just obs -> Just $ augmentChase config theory gstar obs

modelUp :: SATIteratorType -> Maybe (SATIteratorType, Model)
modelUp sat = case upModel sat of
  (Nothing, _) -> Nothing
  (Just mdl', stack') -> Just (stack', mdl')

modelDown :: SATIteratorType -> Maybe (SATIteratorType, Model)
modelDown sat = Nothing
    
type QBlame = Either Error (Blame, Sequent)
--
--    
getJustification :: ChaseState -> Model -> Formula -> QBlame
getJustification gstar@(b,p,t,c) mdl fml = case getObservation fml of
  Nothing -> Left "blame formula is not an observation"
  Just obv -> case getObservationBlame (observationProvs p) mdl obv of
    Nothing -> Left "no provenance info for blame observation"
    Just blame -> case getBlamedSequent t blame of
      Nothing -> Left "unable to find blamed theory sequent from provenance info"
      Just bseq -> Right (blame, bseq)

data QOrigin = QOriginLeaf Term QBlame | QOriginNode Term QBlame [QOrigin]
--
--
getOrigin :: Theory -> ChaseState -> Model -> Bool -> Term -> [QOrigin]
getOrigin thy gstar@(b,p,t,c) mdl isrec term = do
  case name of
    Left err -> [QOriginLeaf term (Left err)]
    Right origins -> do
      (origin, nextelms) <- origins
      case isrec of
        False -> return $ QOriginLeaf term (blamed origin)
        True -> return $ QOriginNode term (blamed origin) (concatMap (\e->(getOrigin thy gstar mdl isrec (Elem e))) nextelms)
  where 
    name = case getEqualElements mdl term of
      [] -> Left $ "element "++(show term)++" not in the current model"
      eqelms -> do
        case getElementBlames thy (elementProvs p) mdl eqelms of
          [] -> Left $ "no provenance information for element "++(show term)
          origins -> Right origins
    blamed origin = case getBlamedSequent t origin of
      Nothing -> Left $ "unable to find blamed theory sequent from provenance info\n"++(show origin)
      Just bseq -> Right (origin, bseq)
