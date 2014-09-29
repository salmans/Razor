{- Razor
   Module      : Common.IModel
   Description : This module implements a data-structure for models. 
   Maintainer  : Salman Saghafi -}
module Common.IModel where

-- Standard
import Data.List (partition, groupBy, intercalate, sort)

-- Control
import Control.Applicative

-- Syntax
import Syntax.GeometricUtils ( FnSym, RelSym, Element (..) 
                             , Term (..), Atom (..))

-- Common
import Common.Observation (Observation (Obs))
import Common.Provenance (ProvInfo, emptyProvInfo )

{-| A model consists of a set of a list of 'Observation's for facts and an 
  instance of 'ProvInfo' for provenance information for elements and 
  observations.-}
data Model = Model { modelObservations :: [Observation]
                   , modelProvInfo     :: ProvInfo
                   }

{-| Empty Model -}
emptyModel :: Model
emptyModel =  Model [] emptyProvInfo

{- Show Instance for Model -}
instance Show Model where
    show = prettyModel

{- Displaying Models -}
prettyModel :: Model -> String
prettyModel mdl@(Model obs _) = 
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