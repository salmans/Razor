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

module Common.Model where

-- Standard
import Data.List
import qualified Data.Map as Map
import Data.Maybe

-- Control
import Control.Applicative

-- Syntax
import Syntax.GeometricUtils ( FnSym, RelSym, Variable, Element (..)
                             , Term (..), Atom (..)
                             , isVariable, isElement
                             , termToVariable, termToElement)

import qualified Syntax.FirstOrder as FO

-- Common
import Common.Observation (Observation (Obs))

import Tools.Utils (deleteByIndex)

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
    show = showModel

{- Displaying Models -}
showModel :: Model -> String
showModel mdl@(Model eqs obs) = 
    let (elemObs, otherObs) = partition chooseElements obs
        groupedObs          = groupBy sameRelation $ sort otherObs
    in  showDomain elemObs ++ "\n" ++ 
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
showDomain :: [Observation] -> String
showDomain obs = let elems = (\(Obs (Rel "@Element" [e])) -> show e) <$> obs
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


data QueryResult = QRTable [Variable] [[Element]]
                 | QRFull
                 | QREmpty
     deriving Show

joinQR :: QueryResult -> QueryResult -> QueryResult
joinQR QRFull qr  = qr -- hanlde Truth
joinQR qr QRFull  = joinQR QRFull qr

joinQR QREmpty qr = QREmpty -- handle Falsehood
joinQR qr QREmpty = joinQR QREmpty qr

joinQR (QRTable vs1 tbl1) (QRTable vs2 tbl2) =
  let vsInBoth = intersect vs1 vs2
      inds1    = fromJust <$> (\v -> elemIndex v vs1) <$> vsInBoth
      inds2    = fromJust <$> (\v -> elemIndex v vs2) <$> vsInBoth
      rsInBoth = [ (r1, r2) | r1 <- tbl1, r2 <- tbl2
                            , recordsMatch r1 r2 inds1 inds2]
      vs       = mergeTuples vs1 vs2 inds2
      tbl      = (\(r1, r2) -> mergeTuples r1 r2 inds2) <$> rsInBoth
  in  QRTable vs tbl
  where recordsMatch r1 r2 inds1 inds2 = let es1 = (\i -> r1 !! i) <$> inds1
                                             es2 = (\i -> r2 !! i) <$> inds2
                                         in  es1 == es2
        mergeTuples  t1 t2 delInds = 
          t1 ++ (foldr (\i ls -> deleteByIndex i ls) t2 delInds)


query :: Model -> FO.Formula -> QueryResult
query _ FO.Tru  = QRFull
query _ FO.Fls  = QREmpty
query mdl (FO.Atm (FO.Rel sym ts)) -- For now
      | all (\t -> isVariable t || isElement t) ts = 
            let (vTerms, eTerms) = partition isVariable ts
                elmInds = fromJust <$> (\e -> elemIndex e ts) <$> eTerms
                tups = [ fromJust <$> termToElement <$> es |
                         (Obs (Rel s es)) <- modelObservations mdl
                         , s == sym
                         , recordsMatch es eTerms elmInds]
            in  QRTable (fromJust <$> termToVariable <$> vTerms) tups
      | otherwise = undefined
  where recordsMatch r es inds = all (\i -> r !! i == es !! i) inds
      
query mdl (FO.Not fmla) =
  let qr  = query mdl fmla
      dom = Map.keys $ modelElements mdl
  in  case qr of
        QRFull  -> QREmpty
        QREmpty -> QRFull
        QRTable vs tbl -> QRTable vs $ (allTuples dom (length vs)) \\ tbl
query mdl (FO.And fmla1 fmla2) =
  let qr1 = query mdl fmla1
      qr2 = query mdl fmla2
  in  joinQR qr1 qr2
query _ (FO.Or _ _ ) = undefined
query _ (FO.Imp _ _) = undefined
query mdl (FO.Exists _ x fmla) =
  let qr = query mdl fmla
  in  case qr of
        QRFull  -> QRFull
        QREmpty -> QREmpty
        QRTable vs tbl ->
                let ind = elemIndex x vs
                in  case ind of
                      Nothing -> qr
                      Just i  -> let vs'  = deleteByIndex i vs
                                     tbl' = (deleteByIndex i) <$> tbl
                                 in  case vs' of
                                       [] -> QRFull
                                       _  -> QRTable vs' tbl'
                                       
-- Helper
allTuples :: [Element] -> Int -> [[Element]]
allTuples es 1 = [return e | e <- es]
allTuples es n = [e:rest | e <- es, rest <- allTuples es (n - 1)]
