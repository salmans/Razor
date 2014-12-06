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
data RazorState = RazorState Config (Maybe Theory) (Maybe ChaseState) (Maybe SATIteratorType) (Maybe Model)
type ChaseState = (ChasePossibleFactsType, ProvInfo, SATTheoryType, Int)
type Error = String

setupState :: IO RazorState
setupState = do 
  args <- getArgs
  config <- parseConfig args
  return $ RazorState config Nothing Nothing Nothing Nothing

teardownState :: RazorState -> ()
teardownState state@(RazorState config theory gstar sat model) = case sat of
  Nothing -> ()
  Just opensat -> closeSAT opensat

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
            let gs = generateGS config {configSkolemDepth = dps} thy
            return $ Right (thy, gs)
          Nothing -> return $ Left $ "Unable to parse input theory!"

modelNext :: Either (Config, SATTheoryType) SATIteratorType -> Maybe (SATIteratorType, Model)
modelNext seed = case seed of
  Left (config, t) -> case nextModel (openSAT config t) of
    (Nothing, _) -> Nothing
    (Just mdl', stream') -> Just (stream',mdl')
  Right stream -> case nextModel stream of
    (Nothing, _) -> Nothing
    (Just mdl', stream') -> Just (stream',mdl')

modelUp :: Config -> Theory -> ChaseState -> Formula -> Maybe ChaseState
modelUp config theory gstar@(b,p,t,c) fml = case getObservation fml of
  Nothing -> Nothing
  Just obs -> Just $ augment config theory gstar obs

modelDown :: SATIteratorType -> Maybe (SATIteratorType, Model)
modelDown sat = case undoAndNext sat of
  (Nothing, _) -> Nothing
  (Just mdl', stack') -> Just (stack', mdl')
    
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
