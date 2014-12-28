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

  Module      : Common.IModel
  Description : This module implements a data-structure for models. 
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Common.IModel where

-- Standard
import Data.List (partition, groupBy, intercalate, sort, nub)
import qualified Data.Map as Map

-- Control
import Control.Applicative

-- Syntax
import Syntax.GeometricUtils ( FnSym, RelSym, Element (..)
                             , Term (..), Atom (..))

-- Common
import Common.Observation (Observation (Obs))

import Tools.Trace

{-| A model consists of a list of 'Observation's and a map from a representative
  element to a list of elements that are in the same euivalence class. -}
data Model = Model { modelElements     :: Map.Map Element [Element]
                   , modelObservations :: [Observation] }

{-| Empty Model -}
emptyModel :: Model
emptyModel =  createModel Map.empty []

{-| createModel is the primary constructor for 'Model'. -}
createModel :: Map.Map Element [Element] -> [Observation] -> Model
createModel elemMap obss = 
    let obss' = normalizeObservations elemMap obss
    in  Model elemMap $ nub obss'

{- Normalizes a list of observations based on the equivalence relation among the
   elements of the model. -}
normalizeObservations :: Map.Map Element [Element] -> [Observation]
                      -> [Observation]
normalizeObservations elemMap obss = 
    let list  = Map.toList elemMap
        fList = concat $ (\(k, es) -> (\e -> (e, k)) <$> es) <$> list
        fMap  = Map.fromList fList
    in normalizeObservation fMap <$> obss

normalizeObservation :: Map.Map Element Element -> Observation -> Observation
normalizeObservation elemMap (Obs atm) = Obs $ normalizeAtom elemMap atm

normalizeAtom :: Map.Map Element Element -> Atom -> Atom
normalizeAtom elemMap (Rel s ts)   = Rel s (normalizeTerm elemMap <$> ts)
normalizeAtom elemMap (FnRel s ts) = FnRel s (normalizeTerm elemMap <$> ts)
normalizeAtom _       atm@(Inc _)  = atm

normalizeTerm :: Map.Map Element Element -> Term -> Term
normalizeTerm elemMap (Elem e) = Elem $ Map.findWithDefault e e elemMap
normalizeTerm _       t        = t


{- Show Instance for Model -}
instance Show Model where
    show = prettyModel

{- Displaying Models -}
prettyModel :: Model -> String
prettyModel mdl@(Model eqs obs) = 
    let (elemObs, otherObs) = partition chooseElements obs
        groupedObs          = groupBy sameRelation $ sort otherObs
    in  prettyDomain elemObs ++ "\n" ++ 
        intercalate "\n" (showObservationGroup <$> groupedObs) ++ "\n"
    where chooseElements = (\o -> case o of
                                    Obs (Rel "@Element" _) -> True
                                    otherwise              -> False)

{- Returns true if two 'Observation's are in the same relation. -}
sameRelation :: Observation -> Observation -> Bool
sameRelation (Obs (Rel r1 _) ) (Obs (Rel r2 _))     = r1 == r2
sameRelation (Obs (FnRel r1 _) ) (Obs (FnRel r2 _)) = r1 == r2
sameRelation (Obs (Inc _)) (Obs (Inc _))            = True
sameRelation _ _                                    = False -- otherwise

{- Given a list of @Element observations, returns a string for displaying the 
   elements of the observations as the domain of a model. -}
prettyDomain :: [Observation] -> String
prettyDomain obs = let elems = (\(Obs (Rel "@Element" [e])) -> show e) <$> obs
                   in  "Domain: {" ++ intercalate ", " elems ++ "}"

{- Given a list of observations with the same symbol, returns a string for 
   displaying them. -}
showObservationGroup :: [Observation] -> String
showObservationGroup []  = ""
showObservationGroup obs = case head obs of
                             (Obs (Rel sym _))   -> showRelationObs sym obs
                             (Obs (FnRel sym _)) -> showFunctionObs sym obs
                             (Obs (Inc _))       -> showIncompleteObs   obs

{- Displays a list of relational tuples. -}
showRelationObs :: RelSym -> [Observation] -> String
showRelationObs "=" obs = 
    let obs'   = filter (\(Obs (Rel _ [t1, t2])) -> t1 /= t2) obs
        elems  = (\(Obs (Rel _ ts)) -> showRelationTuple ts) <$> obs'
    in  if   null obs'
        then ""
        else (show "=") ++ " = {" ++ intercalate ", " elems ++ "}"
showRelationObs sym obs = 
    let elems  = (\(Obs (Rel _ ts)) -> showRelationTuple ts) <$> obs
    in  (show sym) ++ " = {" ++ intercalate ", " elems ++ "}"

{- Displays a list of functional tuples. -}
showFunctionObs :: FnSym -> [Observation] -> String
showFunctionObs sym obss  = 
    let elems  = (\(Obs (FnRel _ ts)) -> showFunctionTuple ts) <$> obss
    in  (show sym) ++ " = {" ++ intercalate ", " elems ++ "}"

{- Display an incomplete flag, assuming that all incomplete flags are grouped 
   together. -}
showIncompleteObs :: [Observation] -> String
showIncompleteObs obss =
    let obss' = (\(Obs atm) -> show atm) <$> obss
    in  "{" ++ intercalate "," obss' ++ "}"


{- A helper for 'showRelationObs' for displaying tuples. 
   ** Notice that the funciton displays elements independent of Show instance 
   for 'Element'. -}
showRelationTuple :: [Term] -> String
showRelationTuple [] = ""
showRelationTuple es = "(" ++ 
                       intercalate "," ((\(Elem e) -> show e) <$> es) ++ 
                       ")"

{- A helper for 'showFunctionObs' for displaying tuples. 
   ** Notice that the funciton displays elements independent of Show instance 
   for 'Element'. -}
showFunctionTuple :: [Term] -> String
showFunctionTuple []  = ""
showFunctionTuple [e] = let Elem res = e in show res
showFunctionTuple es  = 
    let args     = init es
        Elem res = last es
    in  "(" ++ 
        intercalate "," ((\(Elem e) -> show e) <$> args) ++ 
        ") -> " ++ show res