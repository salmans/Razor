{- Razor
   Module      : Common.IModel
   Description : This module implements a data-structure for models. 
   Maintainer  : Salman Saghafi -}
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

normalizeTerm :: Map.Map Element Element -> Term -> Term
normalizeTerm elemMap (Elem e) = Elem $ Map.findWithDefault e e elemMap
normalizeTerm _       t        = t


{- Show Instance for Model -}
instance Show Model where
    show = prettyModel

{- Displaying Models -}
prettyModel :: Model -> String
prettyModel mdl@(Model dom obs) = 
    let (elemObs, otherObs) = partition chooseElements obs
        groupedObs          = groupBy sameRelation $ sort otherObs
    in  (show dom) ++ "\n" ++
        prettyDomain elemObs ++ "\n" ++ 
        intercalate "\n" (showObservationGroup <$> groupedObs) ++ "\n"
    where chooseElements = (\o -> case o of
                                    Obs (Rel "@Element" _) -> True
                                    otherwise              -> False)

{- Returns true if two 'Observation's are in the same relation. -}
sameRelation :: Observation -> Observation -> Bool
sameRelation (Obs (Rel r1 _) ) (Obs (Rel r2 _))     = r1 == r2
sameRelation (Obs (FnRel r1 _) ) (Obs (FnRel r2 _)) = r1 == r2
sameRelation _ _                                    = False -- otherwise

{- Given a list of @Element observations, returns a string for displaying the 
   elements of the observations as the domain of a model. -}
prettyDomain :: [Observation] -> String
prettyDomain obs = let elems = (\(Obs (Rel "@Element" e)) -> show e) <$> obs
                   in  "Domain: " ++ intercalate ", " elems

{- Given a list of observations with the same symbol, returns a string for 
   displaying them. -}
showObservationGroup :: [Observation] -> String
showObservationGroup []  = ""
showObservationGroup obs = case head obs of
                             (Obs (Rel sym _))    -> showRelationObs sym obs
                             (Obs (FnRel sym _))  -> showFunctionObs sym obs

{- Displays a list of relational tuples. -}
showRelationObs :: RelSym -> [Observation] -> String
showRelationObs "=" obs = 
    let obs'   = filter (\(Obs (Rel _ [t1, t2])) -> t1 /= t2) obs
        elems  = (\(Obs (Rel _ ts)) -> showTuple ts) <$> obs'
    in  if   null obs'
        then ""
        else (show "=") ++ " = {" ++ intercalate ", " elems ++ "}"
showRelationObs sym obs = 
    let elems  = (\(Obs (Rel _ ts)) -> showTuple ts) <$> obs
    in  (show sym) ++ " = {" ++ intercalate ", " elems ++ "}"

{- Displays a list of functional tuples. -}
showFunctionObs :: FnSym -> [Observation] -> String
showFunctionObs sym obs = let elems  = (\(Obs (FnRel _ ts)) -> 
                                            showTuple ts) <$> obs
                          in  (show sym) ++ " = {" 
                                  ++ intercalate ", " elems ++ "}"

{- A helper for 'showFunctionObs' and 'showRelationObs' for displaying tuples. 
   Note that the funciton displays elements independent of Show instance for 
   'Element'. -}
showTuple :: [Term] -> String
showTuple es = "(" ++ intercalate "," ((\(Elem (Element e)) -> e) <$> es) ++ ")"