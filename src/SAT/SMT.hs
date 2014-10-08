{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

{- Razor
   Module      : SAT.SMT
   Description : The module provides an interface to SBV for SMT translation and
   SMT solving.
   Maintainer  : Salman Saghafi -}

module SAT.SMT where

-- Standard
import Data.List (intercalate, sortBy, groupBy, union)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as Text
import System.IO.Unsafe

-- Control
import Control.Applicative
import Control.Monad
import qualified Control.Monad.State.Lazy as State

-- SBV
import Data.SBV

-- Syntax
import Syntax.GeometricUtils ( FnSym, RelSym, Atom (..), Term (..)
                             , Element (..), Constant (..)
                             , termToElement )

-- Common
import Common.Observation (Observation (..), ObservationSequent (..))
import Common.Model (Model, createModel)

-- SAT
import SAT.Data

-- Tools
import qualified Tools.ExtendedSet as ExSet
import Tools.Trace

-- Error Messages
unitName = "SAT.SMT"
error_InvalidFnArity         = "invalid function arity!"
error_InvalidRelArity        = "invalid relation arity!"
error_FunctionalAtomExpected = "functional atom expected!"
error_RelationalAtomExpected = "relational atom expected!"


{-|Solver Interface Types -}
type SATTheoryType   = SMTTheory
type SATIteratorType = SMT ()
 

{- A name equivalent to an 'Element' in SMT solving -}
type SMTElement = String

{- Creates an 'SMTElement' from an 'Element' -}
smtElement :: Element -> SMTElement
smtElement (Element e) = e

{- A name equivalent to a 'Term' in SMT solving -}
type SMTTerm    = String

{- Creates an instance of 'SMTTerm' from a functional 'Atom'. -}
smtTerm :: Atom -> SMTTerm
smtTerm (FnRel c [_]) = c
smtTerm (FnRel f ts)  = 
    let elms = fromJust <$> termToElement <$> (init ts)
               -- Expecting only flat terms
    in  f ++ "-" ++ (intercalate "-" $ smtElement <$> elms)
smtTerm _            = error $ unitName ++ ".smtTerm: " 
                       ++ error_FunctionalAtomExpected

{- A name equivalent to an 'Atom' in SMT solving -}
type SMTAtom    = String

{- Creates an instance of 'SMTAtom' from a relational 'Atom'. -}
smtAtom :: Atom -> SMTAtom
smtAtom (Rel r []) = smtRelSym r
smtAtom (Rel r ts) = 
    let elms = fromJust <$> termToElement <$> ts
               -- Expecting only flat terms
        r'   = smtRelSym r
    in  r' ++ "-" ++ (intercalate "-" $ smtElement <$> elms)
smtAtom _          = error $ unitName ++ ".smtAtom: " 
                     ++ error_RelationalAtomExpected

{- This function translates special relation symbols to symbols that are 
   accepted by the SMT solver. -}
smtRelSym :: RelSym -> RelSym
smtRelSym "@Element" = "Element0000"
smtRelSym r          = r

relSymFromSMT :: RelSym -> RelSym
relSymFromSMT "Element0000" = "@Element"
relSymFromSMT r             = r

{- SMTObservation represents an 'Observation' in SMT solving:

   [@SMTFact@] represents a relational fact
   [@SMTFunc@] represents a functional fact, holding information about a 
   functional term and its value.
   [@SMTEq@]   denotes equality among two elements.
-} 
data SMTObservation = SMTFact SMTAtom
                    | SMTFn   SMTTerm SMTElement
                    | SMTEq   SMTElement SMTElement
                      deriving Show

instance SATAtom SMTObservation where
    emptySATTheory = emptySMTTheory
    storeSequent   = addToSMTTheory


{- Creates an instance of 'SMTObservation' from an input 'Observation'. -}
smtObservation :: Observation -> SMTObservation
smtObservation (Obs (Rel "=" ts@[t1, t2])) = 
    let es       = fromJust <$> termToElement <$> ts
        [e1, e2] = smtElement <$> es
    in  SMTEq e1 e2
smtObservation (Obs atm@(Rel r ts))        =
    SMTFact (smtAtom atm)
smtObservation (Obs atm@(FnRel f ts))      =
    let e = fromJust $ termToElement (last ts)
    in  SMTFn (smtTerm atm) (smtElement e)


{- SMTObsSequent is a 'SATSequent' for SMTObservation. -}
type SMTSequent = SATSequent SMTObservation

{-| A theory of sequents in SMT solving is a an instance of 'SATTheory' family.
  This type is essentially a wrapper around a computation context of type SMT.  
 -}
data instance SATTheory SMTObservation = SMTTheory (SMT ())

{- A convenient name for working with SMT theories -}
type SMTTheory = SATTheory SMTObservation

{- Empty 'SMTObsTheory' -}
emptySMTTheory :: SMTTheory
emptySMTTheory = SMTTheory (State.put emptySMTContainer)
                 -- The computation context is initialized with an empty 
                 -- instance of SMTContainer. 

{- Converts an 'ObservationSequent' to 'SMTObsSequent' and adds it to an 
   existing SMTTheory -}
addToSMTTheory :: SMTTheory -> ObservationSequent -> SMTTheory
addToSMTTheory (SMTTheory context) seq = 
    SMTTheory (do context
                  addObservationSequent seq)
--------------------------------------------------------------------------------
-- Translation
--------------------------------------------------------------------------------
-- In the current implementation, the state of symbolic information for SMT 
-- solving is stored in a container inside the 'SMT' context. Moreover, the 
-- the constraints for SMT solving are constructed in the 'SMT' context, as 
-- an 'ObservationSequent' is being translated.

{- Translates the input observational sequent inside a symbolic computation of
   type SMT: the computation introduces a constraint to the solver's input. -}
addObservationSequent :: ObservationSequent -> SMT ()
addObservationSequent seq@(ObservationSequent bodies heads) = do
  let bodies'     = filter (filterFunc) bodies
  let heads'      = heads -- (filter filterFunc) <$> heads
  bodiesVals      <- mapM tranObservation bodies'
  headsVals       <- mapM (mapM tranObservation) heads'
  let bodiesValue =  foldr (&&&) true bodiesVals
  let headsValue' =  map (foldr (&&&) true) headsVals
  let headsValue  =  foldr (|||) false headsValue'
  liftSymbolic $ constrain (bodiesValue ==> headsValue)
  where filterFunc b = case b of
                         Obs (FnRel _ [_]) -> False
                         otherwise         -> True

{- Given an input observation, creates a symbolic value of type SBool inside a 
   symbolic computation of type SMT. The result of this function is used by 
   'addObservationSequent' to build a constraint for the input of SMT solver. -}
tranObservation :: Observation -> SMT SBool
tranObservation obs@(Obs (Rel "=" _)) = do
  let smtObs@(SMTEq e1 e2)   = smtObservation obs
  [v1, v2]                  <- mapM elementValue [e1, e2]
  return (v1 .== v2)
tranObservation obs@(Obs atm@(Rel r ts)) = do
  let (SMTFact atom) = smtObservation obs
  let r' = smtRelSym r
  unintRel  <- unintRelValue r' (length ts) 
               -- Not good! symbols must have their arities with themselves. 
               -- This is subject to refactoring. 
  let elms  =  smtElement <$> (\(Elem e) -> e) <$> ts
  vals      <- mapM elementValue elms
  val       <- atomValue r' atom unintRel vals
  return (val .== true)
tranObservation obs@(Obs atm@(FnRel f ts))  = do
  let (SMTFn term elm) = smtObservation obs
                         -- smtObservation drops the last parameter for 
                         -- functions
  let elms  =  smtElement <$> (\(Elem e) -> e) <$> (init ts)
  unintFunc <- unintFnValue f (length elms)
  sIns      <- mapM elementValue elms
  sOut      <- elementValue elm
  val       <- termValue f term unintFunc sIns
  return (val .== sOut)

-- Translating the results back to first-order observations:
{- Converts a solution created by the SMT solver to a set of 'Observation's. -}
translateSolution :: SatResult -> Maybe Model
translateSolution res =
  case res of
    SatResult (Unsatisfiable _) -> Nothing
    SatResult (Satisfiable _ _) -> Just $ translateDictionary 
                                        $ getModelDictionary res
    _ -> error "Z3 SMT Solver not found"

{- A helper for 'translateSolution' -}
translateDictionary :: Map.Map String CW -> Model
translateDictionary dic = 
    let (_, obss)    = Map.partitionWithKey (\k _ -> isElementString k) dic
        (rels, funs) = Map.partition (\cw -> cwKind cw == KBool) obss
        eqClasses    = equivalenceClasses dic
        revDic       = Map.map head eqClasses
        relObss      = Map.elems $ Map.mapWithKey (\k _ -> obsFromSMTAtom k) 
                       (Map.filter (\cw -> fromCW cw == True) rels)
        funObss      = Map.elems $ Map.mapWithKey 
                       (\k cw -> (obsFromSMTTerm k) <$> (Map.lookup cw revDic))
                       funs
        funObss'     = fromJust <$> filter isJust funObss
        domain       = let lists = Map.elems eqClasses
                           pairs = (\l -> (Element (head l), Element <$> l)) 
                                   <$> lists
                       in  Map.fromList pairs
    in  createModel domain $ relObss `union` funObss'


{- Given an 'SMTAtom' string denoting a relational fact, creates an observation.
   In some sense, this funciton undoes what 'smtAtom' does (also converts the
   resulting atom to an observation). -}
obsFromSMTAtom :: SMTAtom -> Observation
obsFromSMTAtom str = 
    let strs = Text.unpack <$> Text.splitOn (Text.pack "-") (Text.pack str)
        sym  = head strs
        es   = tail strs
        sym' = relSymFromSMT sym
    in  Obs $ Rel sym' $ (Elem . Element) <$> es

{- Similar to 'obsFromSMTAtom', but constructs an observatin for a relational 
   fact.  -}
obsFromSMTTerm :: SMTTerm -> SMTElement -> Observation
obsFromSMTTerm str e = 
    let strs = Text.unpack <$> Text.splitOn (Text.pack "-") (Text.pack str)
        sym  = head strs
        es   = tail strs
    in  Obs $ FnRel sym $ (Elem . Element) <$> (es ++ [e])

{- This function constructs equivalence classes on elements of the domain, 
   (represented by 'SMTElement' instances) based on the the result of 
   SMT solving. The input is simply a map from 'SMTElement's to the values that
   they have in the SMT result and the output is their equivalence classes 
   together with their values in the SMT result. 
   This function is useful when
     a. translating the solution to a set of 'Observation's
     b. constructing additional constraints for reducing an initial result from
     the SMT solver to a minimal result. -}
equivalenceClasses :: Map.Map SMTElement CW -> Map.Map CW [SMTElement]
equivalenceClasses dic = 
    let elems    = Map.filterWithKey (\k _ -> isElementString k) dic
        list     = sortBy (\(x, y) (x', y') -> 
                               case compare y y' of
                                 EQ -> compare x x'
                                 c  -> c) $ Map.toList elems
        classes  = groupBy (\(_, x) (_, y) -> x == y) list
    in Map.fromList $ (\c -> (snd (head c), fst <$> c)) <$> classes
--------------------------------------------------------------------------------
-- SBV Implementation
--------------------------------------------------------------------------------
-- Every element, term and atomic formula must be assigned to a value of a type
-- that is supported by SBV. We assign elements (represented by type SMTElement)
-- and terms (of type SMTTerm) to values of type SElement. 

{- Values of elements are of type SWord16 -}
type SElement = SWord16

{- Symbolic domain is a map from 'SMTElement' to their values of type
   symbolic word 'SWord16'. -}
type SDomain = Map.Map SMTElement SElement

{- STerms is a map from 'SMTTerm' to values of type 'SElement' -}
type STerms  = Map.Map SMTTerm SElement

{- SAtoms is a map from 'SMTAtom' to values of type 'SBool' -}
type SAtoms  = Map.Map SMTAtom SBool


{- UninterpretFn presents symbolic functions that are used in SBV for creating 
   the input query to the SMT solver. Because these functions are Haskell 
   functions that operate on symbolic values, their types must be fixed; 
   that is, we cannot have functions of arbitrary arities. -}
data UninterpretFn = UnintFn0 SElement -- Functions of arity 0 are constants
                     | UnintFn1 (SElement -> SElement)
                     | UnintFn2 (SElement -> SElement -> SElement)
                     | UnintFn3 (SElement -> SElement -> SElement -> SElement)
                     | UnintFn4 (SElement -> SElement -> SElement -> SElement 
                                 -> SElement)
                     | UnintFn5 (SElement -> SElement -> SElement -> SElement
                                 -> SElement -> SElement)
                     | UnintFn6 (SElement -> SElement -> SElement -> SElement
                                 -> SElement -> SElement -> SElement)
                     | UnintFn7 (SElement -> SElement -> SElement -> SElement
                                 -> SElement -> SElement -> SElement -> SElement)

{- Show instance for 'UninterpretFn' -}
instance Show UninterpretFn where
    show (UnintFn1 _) = "UnintFn1"
    show (UnintFn2 _) = "UnintFn2"
    show (UnintFn3 _) = "UnintFn3"
    show (UnintFn4 _) = "UnintFn4"
    show (UnintFn5 _) = "UnintFn5"
    show (UnintFn6 _) = "UnintFn6"
    show (UnintFn7 _) = "UnintFn7"

{- Creating 'UninterpretFn' for a given arity -}
uninterpretFn :: FnSym -> Int -> UninterpretFn
uninterpretFn fn 0 = UnintFn0 (uninterpret fn)
uninterpretFn fn 1 = UnintFn1 (uninterpret fn)
uninterpretFn fn 2 = UnintFn2 (uninterpret fn)
uninterpretFn fn 3 = UnintFn3 (uninterpret fn)
uninterpretFn fn 4 = UnintFn4 (uninterpret fn)
uninterpretFn fn 5 = UnintFn5 (uninterpret fn)
uninterpretFn fn 6 = UnintFn6 (uninterpret fn)
uninterpretFn fn 7 = UnintFn7 (uninterpret fn)

{- Returns the arity of a given 'UninterpretFn' -}
unintFnArity :: UninterpretFn -> Int
unintFnArity (UnintFn0 _) = 0
unintFnArity (UnintFn1 _) = 1
unintFnArity (UnintFn2 _) = 2
unintFnArity (UnintFn3 _) = 3
unintFnArity (UnintFn4 _) = 4
unintFnArity (UnintFn5 _) = 5
unintFnArity (UnintFn6 _) = 6
unintFnArity (UnintFn7 _) = 7

{- Applies an instance of 'UninterpretFn' to a list of symbolic arguments -}
applyUnintFn :: UninterpretFn -> [SElement] -> SElement
applyUnintFn (UnintFn0 f) []
    = f -- constant
applyUnintFn (UnintFn1 f) [x1] 
    = f x1
applyUnintFn (UnintFn2 f) [x1, x2] 
    = f x1 x2
applyUnintFn (UnintFn3 f) [x1, x2, x3] 
    = f x1 x2 x3
applyUnintFn (UnintFn4 f) [x1, x2, x3, x4] 
    = f x1 x2 x3 x4
applyUnintFn (UnintFn5 f) [x1, x2, x3, x4, x5]
    = f x1 x2 x3 x4 x5
applyUnintFn (UnintFn6 f) [x1, x2, x3, x4, x5, x6] 
    = f x1 x2 x3 x4 x5 x6
applyUnintFn (UnintFn7 f) [x1, x2, x3, x4, x5, x6, x7] 
    = f x1 x2 x3 x4 x5 x6 x7
applyUnintFn _ _ = error $ unitName ++ ".applyUnintFn: " ++ error_InvalidFnArity

{- Applies an instance of 'UninterpretRel' to a list of symbolic arguments. -}
applyUnintRel :: UninterpretRel -> [SElement] -> SBool
applyUnintRel (UnintRel0 p) []
    = p
applyUnintRel (UnintRel1 f) [x1] 
    = f x1
applyUnintRel (UnintRel2 f) [x1, x2] 
    = f x1 x2
applyUnintRel (UnintRel3 f) [x1, x2, x3] 
    = f x1 x2 x3
applyUnintRel (UnintRel4 f) [x1, x2, x3, x4] 
    = f x1 x2 x3 x4
applyUnintRel (UnintRel5 f) [x1, x2, x3, x4, x5]
    = f x1 x2 x3 x4 x5
applyUnintRel (UnintRel6 f) [x1, x2, x3, x4, x5, x6] 
    = f x1 x2 x3 x4 x5 x6
applyUnintRel (UnintRel7 f) [x1, x2, x3, x4, x5, x6, x7] 
    = f x1 x2 x3 x4 x5 x6 x7
applyUnintRel _ _ = error $ unitName ++ ".applyUnintRel: " 
                    ++ error_InvalidRelArity

{- UninterpretRel presents symbolic relations that are used in SBV for creating 
   the input query to the SMT solver. Because these relations are Haskell 
   functions that operate on symbolic values, their types must be fixed; 
   that is, we cannot have relations of arbitrary arities. -}
data UninterpretRel = UnintRel0 SBool
                    | UnintRel1 (SElement -> SBool)
                    | UnintRel2 (SElement -> SElement -> SBool)
                    | UnintRel3 (SElement -> SElement -> SElement -> SBool)
                    | UnintRel4 (SElement -> SElement -> SElement -> SElement
                                 -> SBool)
                    | UnintRel5 (SElement -> SElement -> SElement -> SElement
                                 -> SElement -> SBool)
                    | UnintRel6 (SElement -> SElement -> SElement -> SElement
                                 -> SElement -> SElement -> SBool)
                    | UnintRel7 (SElement -> SElement -> SElement -> SElement
                                 -> SElement -> SElement -> SElement -> SBool)

{- Show instance for 'UninterpretRel' -}
instance Show UninterpretRel where
    show (UnintRel0 _) = "UnintRel0"
    show (UnintRel1 _) = "UnintRel1"
    show (UnintRel2 _) = "UnintRel2"
    show (UnintRel3 _) = "UnintRel3"
    show (UnintRel4 _) = "UnintRel4"
    show (UnintRel5 _) = "UnintRel5"
    show (UnintRel6 _) = "UnintRel6"
    show (UnintRel7 _) = "UnintRel7"

{- Creating 'UninterpretRel' for a given arity -}
uninterpretRel :: RelSym -> Int -> UninterpretRel
uninterpretRel rel 0 = UnintRel0 (uninterpret rel)
uninterpretRel rel 1 = UnintRel1 (uninterpret rel)
uninterpretRel rel 2 = UnintRel2 (uninterpret rel)
uninterpretRel rel 3 = UnintRel3 (uninterpret rel)
uninterpretRel rel 4 = UnintRel4 (uninterpret rel)
uninterpretRel rel 5 = UnintRel5 (uninterpret rel)
uninterpretRel rel 6 = UnintRel6 (uninterpret rel)
uninterpretRel rel 7 = UnintRel7 (uninterpret rel)

{- Returns the arity of a given 'UninterpretRel' -}
unintRelArity :: UninterpretRel -> Int
unintRelArity (UnintRel0 _) = 0
unintRelArity (UnintRel1 _) = 1
unintRelArity (UnintRel2 _) = 2
unintRelArity (UnintRel3 _) = 3
unintRelArity (UnintRel4 _) = 4
unintRelArity (UnintRel5 _) = 5
unintRelArity (UnintRel6 _) = 6
unintRelArity (UnintRel7 _) = 7

{- SMTContainer contains all the symbolic values and names for addressing them 
   in SMT solivng comprising the following:

   [@containerDomain@] contains the set of all elements in the domain
   [@containerFns@] is a map from function symbols to their equivalent symbolic
   uninterpreted functions.
   [@containerRels@] is a map from relation symbols to their equivalent symbolic
   uninterpreted functions.
   [@containerTerms@]  is a map from a function symbol to the terms of that
   symbol (of type 'STerms') 
   [@containerAtoms@]  is a map from a relation symbol to the atoms of that
   symbol (of type 'SAtoms')  -}
data SMTContainer     = SMTContainer 
    { containerDomain :: SDomain
    , containerFns    :: Map.Map FnSym  UninterpretFn
    , containerRels   :: Map.Map RelSym UninterpretRel
    , containerTerms  :: STerms
    , containerAtoms  :: SAtoms 
    }

{- Initial empty container -}
emptySMTContainer :: SMTContainer 
emptySMTContainer  = 
    SMTContainer Map.empty Map.empty Map.empty Map.empty Map.empty

{- 'SMT' is the computation context for translation to SMT and storing SMT 
   queries, a context for sending 'Observation's to symbolic values and running 
   the SMT solver. -}
type SMT = State.StateT SMTContainer Symbolic

{- Lifting functions for SMT -}
liftContainer = id

liftSymbolic :: (Monad m, State.MonadTrans t) => m a -> t m a
liftSymbolic  = liftContainer.State.lift

{- Returns the symbolic value of an element name of type 'SMTElement' inside an
   SMT computation. The symbolic value will be fetched from the 'SMTContaiener'
   inside the computation or will be created (and inserted into the container)
   if it doesn't exist. -} 
elementValue :: SMTElement -> SMT SElement
elementValue elm = do
  container <- liftContainer State.get
  let domMap = containerDomain container
  sym       <- case Map.lookup elm domMap of
                 Nothing -> liftSymbolic $ sWord16 elm
                 Just s  -> return s
  let domMap' = Map.insertWith (flip const) elm sym domMap
  liftContainer $ State.modify (\c -> c { containerDomain = domMap' })
  return sym

{- Returns the symbolic relation for a relation symbol of the given arity inside
   an SMT computation. This creates a new UninterpretRel value if it already 
   does not exists in the container of computation.-}
unintRelValue :: RelSym -> Int -> SMT UninterpretRel
unintRelValue rel arity = do
  container   <- liftContainer State.get
  let relMap   = containerRels container
  let unintRel = Map.findWithDefault (uninterpretRel rel arity) rel relMap
  -- update container:
  let relMap'  = Map.insertWith (flip const) rel unintRel relMap
                 -- do not insert if exists
  liftContainer $ State.modify (\c -> c { containerRels = relMap' })
  return unintRel

{- Returns the symbolic function for a function symbol of the given arity inside
   an SMT computation. This creates a new UninterpretRel value if it already
   does not exists in the container of computation.-}
unintFnValue :: FnSym -> Int -> SMT UninterpretFn
unintFnValue fn arity = do
  container     <- liftContainer State.get
  let fnMap     = containerFns container
  let unintFunc = Map.findWithDefault (uninterpretFn fn arity) fn fnMap
  -- update container:
  let fnMap'    = Map.insertWith (flip const) fn unintFunc fnMap
                 -- do not insert if exists
  liftContainer $ State.modify (\c -> c { containerFns = fnMap' })
  return unintFunc

{- For an atomic relation of type 'SMTAtom', returns a symbolic boolean value
   inside an SMT computation. If such value does not exists, it will apply the
   given uninterpreted function to the given symbolic elements to create the
   value and add it to the container of computation. -}
atomValue :: RelSym -> SMTAtom -> UninterpretRel -> [SElement] -> SMT SBool
atomValue rel term unintRel sParams = do
  container   <- liftContainer State.get
  let atomMap =  containerAtoms container
  sym         <- case Map.lookup term atomMap of
                   Nothing -> do
                     s <- liftSymbolic $ sBool term
                     liftSymbolic $ constrain 
                              $ (applyUnintRel unintRel sParams) .== s
                     return s
                   Just s  -> return s
  -- update container:
  let atomMap' = Map.insertWith (flip const) term sym atomMap
                 -- do not insert if exists
  liftContainer $ State.modify (\c -> c { containerAtoms = atomMap' })
  return sym

{- For an functional term of type 'SMTTerm', returns a value of type 'SElement'
   inside an SMT computation. If such value does not exists, it will apply the
   given uninterpreted function to the given symbolic elements to create the
   value and add it to the container of computation. -}
termValue :: FnSym -> SMTTerm -> UninterpretFn -> [SElement] -> SMT SElement
termValue fn term unintFunc sParams = do
  container   <- liftContainer State.get
  let termMap  = containerTerms container
  sym         <- case Map.lookup term termMap of
                   Nothing -> do
                     s <- liftSymbolic $ sWord16 term
                     liftSymbolic $ constrain 
                              $ (applyUnintFn unintFunc sParams) .== s
                     return s
                   Just s  -> return s
  -- update container:
  let termMap' = Map.insertWith (flip const) term sym termMap 
                 -- do not insert if exists
  liftContainer $ State.modify (\c -> c { containerTerms = termMap' })
  return sym

--------------------------------------------------------------------------------
-- SMT Solving and Model Generation
-- !! The efficiency of this section may be improved.
--------------------------------------------------------------------------------
{- Defining a SATSolver instance that does SMT solving. The 'SMTAtom' type of 
   this implementation is SMTObservation and the type of SMT solving data is 
   SMT (). -}
instance SATSolver SMTObservation (SMT ()) where
    satInitialize (SMTTheory context)      = context
    satSolve context = let (res, context') = minimumResult context
                       in  (translateSolution res, context')

{- Given a query of type SMT, executes the SMT and returns the result. It also
   returns a new 'SMT' computation where the homomorphism cone of the minimum 
   result is eliminated. -}
minimumResult :: SMT () -> (SatResult, SMT ())
minimumResult context = 
    let context'   = do
          context
          liftSymbolic $ solve []
        
        run        = State.evalStateT context' emptySMTContainer
        res        = unsafePerformIO $ satWith z3 run
        minRes     = fst $ reduce context res
        newContext = do
          context
          constraint <- nextResult minRes
          liftSymbolic $ constrain constraint
    in  (minRes, newContext)

{- This recursively reduces the initial result of the SMT solver to construct a 
   minimal model based on an Aluminum-like algorithm. The result of the function
   is the minimal result as well as a new computation context that contains the
   additional constraints for reducing the input result. -}
reduce :: SMT() -> SatResult -> (SatResult, SMT ())
reduce context res@(SatResult (Satisfiable _ _)) = 
    let context'  = do
          context
          constraint <- minimizeResult res
          liftSymbolic $ constrain constraint
        run       = State.evalStateT 
                    (do context'
                        liftSymbolic $ solve [])
                    emptySMTContainer
        res'      = unsafePerformIO $ satWith z3 run
    in  case res' of
          SatResult (Unsatisfiable _) -> (res, context')
          SatResult (Satisfiable _ _) -> reduce context' res'
reduce context res = (res, context)

{- Constructs the a set of constraints for minimizing the result returned by the
   SMT solver and returns their corresponding value of type SBool in the context
   of an SMT computation. These constraints are four-fold:
   
     1. Negative Axioms: these constraints force the negative facts in the 
     current instance of 'SatResult' to remain false. For instance, if 
     @P(e1, e2)@ is not true in the current result, it should never become true 
     after minimization. 
     2. Equality Negative Axioms: these axioms are similar to Negative Axioms 
     but are constructed for equations among elements. For instance, if 
     @e1 /= e2@ in the current result, they must not become equal after 
     minimization.
     3. Flip Axioms: the purpose of this group of axioms is to examine whether
     any of the positive facts in the current model can be ignored or not. For 
     instance, if @P(e1, e2)@ is true in the current model, the Flip Axiom tests
     the possibility of making @P(e1, e2)@ false after minimization.
     4. Equality Flip Axioms: these axioms are similar to Flip Axioms but they
     are applied on equations in the current instance of 'SatResult'.

   The four set of axioms above are combined as the following:
   Negative Axioms & 
   Equality Negative Axioms & 
   (Flip Axioms | Equality Flip Axioms)
-}
minimizeResult :: SatResult -> SMT SBool
minimizeResult res = do
  container   <- liftContainer State.get
  let domain   = containerDomain container
  let rels     = Map.toList $ containerRels container
  let dic      = getModelDictionary res
  let classes  = equivalenceClasses dic
  let sClasses = ((\e -> fromJust $ Map.lookup e domain) <$>) 
                 <$> (Map.elems classes)
  let factStrs = Map.keys $ Map.filter (\s -> fromCW s == True ) dic
  let termStrs = Map.toList $ Map.filterWithKey
                 (\k cw -> cwKind cw /= KBool && not (isElementString k)) dic

  let symAtoms = Map.toList $ smtSymAtomMap factStrs
  let eqNegAx  = equalityNegativeAxioms sClasses
  let eqFlipAx = foldr (|||) false $ equalityFlipAxioms <$> sClasses
  negs  <- mapM (\(r, unintRel) -> 
                     let as = fromMaybe [] (lookup r symAtoms)
                     in  negativeAxioms unintRel as) rels
  fnNeg <- functionNegativeAxioms termStrs classes
  fnFlips <- functionFlipAxioms termStrs classes
  flips <- mapM (\(r, unintRel) -> 
                     let as = fromMaybe [] (lookup r symAtoms)
                     in  flipAxioms unintRel as) rels
  let negAx    = foldr (&&&) true negs
  let flipAx   = foldr (|||) false flips
  return $ fnNeg &&& negAx &&& eqNegAx &&& (flipAx ||| fnFlips ||| eqFlipAx)

{- This function creates Negative Axioms for a set of facts that are named by
   a list of SMTAtom instances: every SMTAtom names a *positive* fact in a 
   result from the SMT solver. In order to improve the efficiency of 
   computation, the function assumes that all the SMTAtom instances belong to
   a relation whose symbolic uninterpreted value is also given to the function. 
   This function will be called once for every relational symbol in the 
   container of the current SMT computation.

   Example. Assuming that the function is called for relation @R@ and two 
   positive atomic facts @[R(e1, e2), R(e2, e3)]@, it creates an axiom like the
   following:
   @forall x, y. ~R(x, y) | (x = e1 & y = e2) | (x = e2 & y = e3)@
-}
negativeAxioms :: UninterpretRel -> [SMTAtom] -> SMT SBool
negativeAxioms unintRel atoms = do
  container <- liftContainer State.get
  let arity  = unintRelArity unintRel
  vars      <- replicateM arity $ liftSymbolic forall_
  allArgs   <- mapM symAtomArgs atoms
  let pairs  = map (\args -> zipWith (.==) vars args) allArgs
  let cons   = map (foldr (&&&) true) pairs
  let disj   = foldr (|||) false cons  
  return $ (bnot (applyUnintRel unintRel vars)) ||| disj

functionNegativeAxioms :: [(SMTTerm, CW)] -> Map.Map CW [SMTElement] -> SMT SBool
functionNegativeAxioms terms classes = do
  container    <- liftContainer State.get
  let domain    = containerDomain container
  let contTerms = containerTerms container
  let classes'  = 
          Map.map (\es -> (\e -> fromJust $ Map.lookup e domain) <$> es) classes
  let content   = (\(t, cw) -> ( fromJust $ Map.lookup t contTerms
                               , otherClasses cw classes')) <$> terms
  let results   = (uncurry functionNegativeAxiomsHelper) <$> content
  return $ foldr (&&&) true results
    where otherClasses c cs = Map.elems $ Map.delete c cs

functionNegativeAxiomsHelper :: SElement -> [[SElement]] -> SBool
functionNegativeAxiomsHelper term classes = do
  let eqs   = concatMap ((\e -> e ./= term) <$>) classes
  foldr (&&&) true eqs

functionFlipAxioms :: [(SMTTerm, CW)] -> Map.Map CW [SMTElement] -> SMT SBool
functionFlipAxioms terms classes = do
  container    <- liftContainer State.get
  let domain    = containerDomain container
  let contTerms = containerTerms container
  let classes'  = 
          Map.map (\es -> (\e -> fromJust $ 
                                 Map.lookup e domain) <$> es) classes
  let content   = (\(t, cw) -> ( fromJust $ 
                                 Map.lookup t contTerms
                               , thisClass cw classes')) <$> terms
  let results   = (uncurry functionFlipAxiomsHelper) <$> content
  return $ foldr (|||) false results
    where thisClass c cs = Map.findWithDefault [] c cs
  
functionFlipAxiomsHelper :: SElement -> [SElement] -> SBool
functionFlipAxiomsHelper term cls = do
  let eqs   = (\e -> e ./= term) <$> cls
  foldr (|||) false eqs

{- This function creates Flip Axioms for a set of facts that are named by a list
   of SMTAtom instances. Just like 'negativeAxioms', the function assumes that
   all the SMTAtom instances belong to the same relation whose symbolic 
   uninterpreted function is given to the function. -}
flipAxioms :: UninterpretRel -> [SMTAtom] -> SMT SBool
flipAxioms unintRel atoms = do
  container   <- liftContainer State.get
  allArgs     <- mapM symAtomArgs atoms
  let negApps  = bnot.(applyUnintRel unintRel) <$> allArgs
  return $ foldr (|||) false negApps

{- Given a set of equivalence classes on all elements, creates the set of 
   Equality Negative Axioms. The function recursively makes sure that the 
   elements of an equivalence class will never be equal to the elements of 
   other equivalence class. -}
equalityNegativeAxioms :: [[SElement]] -> SBool
equalityNegativeAxioms []            = true
equalityNegativeAxioms (cls:classes) =
    let this = foldr (&&&) true [differentClasses cls cls' | cls' <- classes]
    in  this &&& equalityNegativeAxioms classes

{- As a helper for 'equalityNegativeAxioms', creates a constraint for two sets 
   of elements in different equivalence classes that prevents them from being
   equal after minimization. -}
differentClasses :: [SElement] -> [SElement] -> SBool
differentClasses cls1 cls2 =
    foldr (&&&) true [e1 ./= e2 | e1 <- cls1, e2 <- cls2]

{- Given an equivalence class of elements, creates an axiom to test whether any 
   of the two elements in the class may be unequal after minimization or not. -}
equalityFlipAxioms :: [SElement] -> SBool
equalityFlipAxioms []     = false
equalityFlipAxioms (e:es) = 
    let this = foldr (|||) false [e ./= e' | e' <- es]
    in  this ||| equalityFlipAxioms es

{- Given a list of atom names of type 'SMTAtom', returns a map from the relation
   symbols to their corresponding atom names.  -}
smtSymAtomMap :: [SMTAtom] -> Map.Map String [SMTAtom]
smtSymAtomMap atoms = 
    let list = (\a -> (symOf a, [a])) <$> atoms
    in  Map.fromListWith (++) list
    where symOf atm = Text.unpack 
                      $ head (Text.splitOn (Text.pack "-") (Text.pack atm))

{- When iterating through minimal models, we need additional constraints to
   ensure that the next models returned by the SMT solver are not in the 
   homomorphism cone of any of the previously generated models. This function
   adds a set of Flip Axioms and a set of Equality Flip Axioms to eliminate the
   homomorphism cone of the given 'SatResult' instance that corresponds to a 
   minimal model.
-}
nextResult :: SatResult -> SMT SBool
nextResult res = do
  container   <- liftContainer State.get
  let domain   = containerDomain container
  let rels     = Map.toList $ containerRels container
  let dic      = getModelDictionary res
  let termStrs = Map.toList $ Map.filterWithKey
                 (\k cw -> cwKind cw /= KBool && not (isElementString k)) dic
  let classes  = equivalenceClasses dic
  let sClasses = ((\e -> fromJust $ Map.lookup e domain) <$>) 
                 <$> (Map.elems classes)
  let factStrs = Map.keys $ Map.filter (\s -> fromCW s == True ) dic
  let symAtoms = Map.toList $ smtSymAtomMap factStrs
  let eqFlipAx = foldr (|||) false $ equalityFlipAxioms <$> sClasses
  flips <- mapM (\(r, unintRel) -> 
                     let as = fromMaybe [] (lookup r symAtoms)
                     in  flipAxioms unintRel as) rels
  let flipAx   = foldr (|||) false flips
  funFlipAx   <- functionFlipAxioms termStrs classes
  return $ flipAx ||| eqFlipAx ||| funFlipAx
  
{- Given the name of an atomic fact as an instance of type 'SMTAtom', returns 
   the symbolic values of the arguments of the fact in the current SMT 
   computation.  -}
symAtomArgs :: SMTAtom -> SMT [SElement]
symAtomArgs smtAtom = do
  container   <- liftContainer State.get
  let domain   = containerDomain container
  let (_:args) = Text.unpack <$> 
                 Text.splitOn (Text.pack "-") (Text.pack smtAtom)
  return $ (\elm -> fromJust $ Map.lookup elm domain) <$> args

{- Does the name of a symbolic value correspond to an element of the model? -}
isElementString :: String -> Bool
isElementString ('e':'#':_) = True
isElementString _           = False