{- Razor
   Module      : Common.IObservation
   Description : This module defines and implements 'Observation' and its 
   related functions. 'Observation' is the unit of information in models.
   Maintainer  : Salman Saghafi -}
module Common.IObservation where

-- Standard
import Data.List (intercalate)
import Data.Maybe (catMaybes)

-- Control
import Control.Applicative

-- Syntax
import Syntax.GeometricUtils ( Sequent (..), Formula (..), Atom (..), Term (..)
                             , RelationBased (..), Constant (..)
                             , isVariable, isConstant)

-- Common
import Common.Data (SequentLike (..))

-------------------------
unitName = "Common.Observation"
error_innerDisjunction   = "disjunctions can appear only at the top level."
error_invalidSequent     = "Invalid Sequent!"
error_invalidObservation = "the observation is not a closed fact"


{-| An observation is an atomic fact. -}
data Observation = Obs Atom
                   deriving (Eq, Ord)

{- Show instance for 'Observation'. -}
{- __   __
   <o> <o>   --> showing "observations" in angle brackets
      |
     ---  -}
instance Show Observation where
    show (Obs a)     = "<" ++ (show a) ++ ">"

instance RelationBased Observation where
    relationSyms (Obs (Rel sym ts))   = [(sym, length ts)]
    relationSyms (Obs (FnRel sym ts)) = [(sym, length ts)]

{- Converts an atomic fact (over elements only) to an 'Observation'. -}
toObservation :: Atom -> Maybe Observation
toObservation a@(Rel sym ts)   = 
    if   any hasVarOrConst ts
    then Nothing
    else Just $ Obs a
    where hasVarOrConst (Fn _ ts) = any hasVarOrConst ts
          hasVarOrConst t         = isVariable t || isConstant t -- otherwise
toObservation a@(FnRel sym ts) = 
    if   any hasVarOrConst ts
    then Nothing
    else Just $ Obs a
    where hasVarOrConst (Fn _ ts) = any hasVarOrConst ts
          hasVarOrConst t         = isVariable t || isConstant t -- otherwise

{-| An observational sequent is a closed (no free variables) instance of a 
  'Sequent'. 

  [@observationSequentBody@] :  returns the body of an 'ObservationSequent'
  where every element of the list of 'Observation' is a conjunct in the body of
  a sequent in its standard form. 
  [@observationSequentHead@] :  returns the head of an 'ObservationSequent'
  where every inner list of 'Observation' is the conjunction of a set of 
  observations and every element of the outer list is a disjunct.
-}
data ObservationSequent = ObservationSequent 
    { observationSequentBody :: [Observation]
    , observationSequentHead :: [[Observation]]
    } deriving (Eq)

instance Show ObservationSequent where
    show (ObservationSequent bdy hd) =
        (show bdy) ++ " -> " ++ showHead ++ "\n"
        where showHead = intercalate " \\/ " (show <$> hd)

instance SequentLike ObservationSequent where
     toSequent        = destroyObservationSequent
     fromSequent      = buildObservationSequent
     startSequent     = (null.observationSequentBody)
     failSequent      = (null.observationSequentHead)
     skolemFunctions  = const [] -- They should not have any
     sequentConstants = const []

{-| Creates a Sequent from the given ObservationSequent -}
destroyObservationSequent :: ObservationSequent -> Sequent
destroyObservationSequent (ObservationSequent bdy hds) =
  Sequent (processConjuncts bdy) (processDisjuncts hds)
{-| Creates an 'ObservationSequent' form an input 'Sequent'. -}
buildObservationSequent :: Sequent -> Maybe ObservationSequent
buildObservationSequent seq@(Sequent bdy hds) = 
  ObservationSequent <$> processBody bdy <*> processHead hds

-- Converts a single observational atom into it's user provided form, or nothing
processAtom :: Atom -> Maybe Formula
processAtom (Rel ('@':rsym) terms) = Nothing
processAtom (FnRel fsym terms) = Just $ Atm (Rel "=" [(Fn fsym (init terms)), (last terms)])
processAtom atm = Just $ Atm atm
-- Converts observational atoms into the form the user originally provided them in
-- If they are extra additions, they are removed
processAtoms :: [Observation] -> [Formula]
processAtoms atms = catMaybes $ map (\(Obs atm) -> processAtom atm) atms
-- Parses the inner list of an observational formula, which is a list of atoms separated by conjunction
processConjuncts :: [Observation] -> Formula
processConjuncts [] = Tru
processConjuncts cs = foldl1 (\f1 f2 -> And f1 f2) $ processAtoms cs
-- Parses the outer list of an observational formula, which is a list of conjuncts separated by disjunction
processDisjuncts :: [[Observation]] -> Formula
processDisjuncts (d:[]) = processConjuncts d
processDisjuncts (d:ds) = Or (processConjuncts d) (processDisjuncts ds)
{- Constructs the head of an 'ObservationSequent' from the head of a sequent. 
   Here, we assume that the sequent is in the standard form, i.e. disjunctions 
   appear only at the top level of the head.
 -}
processHead :: Formula -> Maybe [[Observation]]
processHead Fls             = Just []
processHead (And p q)       = 
    let p' = processHead p
        q' = processHead q
    in case (p',q') of
         -- Because disjunction appears only at the top level, the result of
         -- applying this function to a conjunct must be a list with only one
         -- element:
         (Just [], Just [])       -> Just []
         (Just [p''], Just [q'']) -> Just [p'' ++ q'']
         (Just [p''], Just [])    -> Just []
         (Just [], Just [q''])    -> Just []
         (Nothing, _         )    -> Nothing
         (_      , Nothing   )    -> Nothing
         otherwise      -> error $ unitName 
                           ++ ".processHead: " 
                           ++ error_innerDisjunction
processHead (Or p q)        = 
    let res = case (processHead p, processHead q) of
                (Nothing, Nothing) -> Nothing
                (p'     , Nothing) -> p'
                (Nothing, q'     ) -> q'
                (p'     , q'     ) -> (++) <$> p' <*> q'
    in  filter (not.null) <$> res
processHead (Exists _ _ p)  = processHead p
processHead (Lone _ _ p _)  = processHead p
processHead (Atm atm)       = pure <$> pure <$> toObservation atm
processHead _               = error $ unitName 
                              ++ ".processHead: " 
                              ++ error_invalidSequent

{- Constructs the body of an 'ObservationSequent' from the body of 
   a 'Sequent'. Here, we assume that the sequent is in the standard form, i.e. 
   the body does not have disjunctions or existential quantifiers.
 -}
processBody :: Formula -> Maybe [Observation]
processBody Tru            = Just []
processBody (And p q)      = (++) <$> processBody p <*> processBody q
processBody (Atm atm)      = pure <$> toObservation atm
processBody (Exists _ _ p) = processBody p
processBody (Lone _ _ p _) = processBody p
processBody _              = error $ unitName 
                             ++ ".processBody: " 
                             ++ error_invalidSequent
