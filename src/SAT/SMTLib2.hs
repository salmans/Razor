{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- Razor
   Module      : SAT.SMTLib2
   Description : The module provides an interface to SMTLib2 library
   (https://github.com/hguenther/smtlib2) for SMT translation and solving.
   Maintainer  : Salman Saghafi -}

module SAT.SMTLib2 where

-- Standard
import Data.List (intercalate, sortBy, groupBy, union, partition)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as Text
import System.IO.Unsafe

-- Control
import Control.Applicative
import Control.Monad
import qualified Control.Monad.State.Lazy as State
import Control.Monad.Trans


-- SMTLib2
import Language.SMTLib2
import Language.SMTLib2.Internals
import Language.SMTLib2.Solver
import Language.SMTLib2.Connection
import Language.SMTLib2.Pipe

-- Syntax
import Syntax.GeometricUtils ( FnSym, RelSym, Atom (..), Term (..)
                             , Element (..), Constant (..)
                             , termToElement, readElement )

-- Common
import Common.Observation (Observation (..), ObservationSequent (..))
import Common.Model (Model, createModel)

-- SAT
import SAT.Data



-- Error Messages
unitName = "SAT.SMT"
error_InvalidFnArity         = "invalid function arity!"
error_InvalidRelArity        = "invalid relation arity!"
error_FunctionalAtomExpected = "functional atom expected!"
error_RelationalAtomExpected = "relational atom expected!"

{-|Solver Interface Types -}
type SATTheoryType   = SMTTheory
type SATIteratorType = MSMT ()

{- A name for an 'Element' in SMT solving -}
type SMTElement = String

{- Creates a name of type 'SMTElement' for an 'Element' -}
smtElement :: Element -> SMTElement
smtElement e = show e

{- A name for a 'Term' in SMT solving -}
type SMTTerm    = String

{- Creates a name of type 'SMTTerm' for a functional 'Term'. -}
smtTerm :: Atom -> SMTTerm
smtTerm (FnRel c [_]) = c
smtTerm (FnRel f ts)  = 
    let elms = fromJust <$> termToElement <$> (init ts)
               -- Expecting only flat terms
    in  f ++ "-" ++ (intercalate "-" $ smtElement <$> elms)
smtTerm _            = error $ unitName ++ ".smtTerm: " 
                       ++ error_FunctionalAtomExpected

{- A name for an 'Atom' in SMT solving -}
type SMTAtom    = String

{- Creates a name of type 'SMTAtom' from a relational 'Atom'. -}
smtAtom :: Atom -> SMTAtom
smtAtom (Rel r []) = smtRelSym r
smtAtom (Rel r ts) = 
    let elms = fromJust <$> termToElement <$> ts
               -- Expecting only flat terms
        r'   = smtRelSym r
    in  r' ++ "-" ++ (intercalate "-" $ smtElement <$> elms)
smtAtom _          = error $ unitName ++ ".smtAtom: " 
                     ++ error_RelationalAtomExpected

{- Translates special relation symbols to symbols that are accepted by the SMT 
   solver. -}
smtRelSym :: RelSym -> RelSym
smtRelSym "@Element" = "Element^" -- bad practice!
smtRelSym r          = r

{- Works in the opposite direction of 'smtRelSym', returns relation symbols from
   their SMT names. The following invariant must be preserved:
   @relSymFromSMT.smtRelSym = id@ -}
relSymFromSMT :: RelSym -> RelSym
relSymFromSMT "Element^" = "@Element"
relSymFromSMT r          = r

{- SMTObservation represents an 'Observation' in SMT solving:
   [@SMTFact@] a relational fact, e.g. @R(ts)@
   [@SMTFunc@] a functional fact, holding information about a functional term 
   and its value, e.g. @f(ts) = e@
   [@SMTEq@]   equality between two elements, e.g. @e1 = e2@
-} 
data SMTObservation = SMTFactObs SMTAtom
                    | SMTFnObs   SMTTerm SMTElement
                    | SMTEqObs   SMTElement SMTElement
                      deriving Show

{- 'SMTObservation' is an instance of 'SATAtom' -}
instance SATAtom SMTObservation where
    emptySATTheory = emptySMTTheory
    storeSequent   = addToSMTTheory

{- Creates an 'SMTObservation' for an input 'Observation'. -}
smtObservation :: Observation -> SMTObservation
smtObservation (Obs (Rel "=" ts@[t1, t2])) = 
    let es       = fromJust <$> termToElement <$> ts
        [e1, e2] = smtElement <$> es
    in  SMTEqObs e1 e2
smtObservation (Obs atm@(Rel r ts))        =
    SMTFactObs (smtAtom atm)
smtObservation (Obs atm@(FnRel f ts))      =
    let e = fromJust $ termToElement (last ts)
    in  SMTFnObs (smtTerm atm) (smtElement e)


{- SMTObsSequent is a 'SATSequent' of SMTObservation. -}
type SMTSequent = SATSequent SMTObservation

{-| A theory of sequents in SMT solving is a an instance of 'SATTheory' family:
  this type is essentially a wrapper around a computation context of type SMT.  
 -}
data instance SATTheory SMTObservation = SMTTheory (MSMT ())

{- A convenient type for working with SMT theories -}
type SMTTheory = SATTheory SMTObservation

{- Empty 'SMTTheory' -}
emptySMTTheory :: SMTTheory
emptySMTTheory = SMTTheory (lift (State.put emptySMTContainer))
                 -- The computation context is initialized with an empty 
                 -- instance of SMTContainer. 

{- Converts an 'ObservationSequent' to 'SMTObsSequent' and adds it to an 
   existing SMTTheory -}
addToSMTTheory :: SMTTheory -> ObservationSequent -> SMTTheory
addToSMTTheory (SMTTheory context) seq = 
    SMTTheory (do context
                  addObservationSequent seq)


{- ResKind determines the type of a value in a SatResult structure. -}
data ResKind  = RKBool | RKInteger
              deriving (Eq, Ord)
{- ResValue represents a value in a SatResult structure. -}
data ResValue = RVBool Bool
              | RVInteger Integer
              deriving (Eq, Ord)
{- Combines ResKind and ResValue to store a value for an expression in a 
   SatResult structure. -}
data Result   = Result { resKind  :: ResKind
                       , resValue :: ResValue}
                       deriving (Eq, Ord)

{- SatDictionary is the primary data stored in SatResult. It is a Map from 
   SMT names to their values of type 'Result'. -}
type SatDictionary = Map.Map String Result

{- SatResult is an abstraction for the models returned by the SMT solver. -}
data SatResult = Unsatisfiable
               | Satisfiable { getSatResultDictionary :: SatDictionary }
--------------------------------------------------------------------------------
-- Translation
--------------------------------------------------------------------------------
{- Translates the input observational sequent inside a symbolic computation of
   type SMT: the computation introduces a constraint to the solver's input. -}
addObservationSequent :: ObservationSequent -> MSMT ()
addObservationSequent seq@(ObservationSequent bodies heads) = do
  let bodies'     =  bodies
  let heads'      =  heads
  bodiesVals      <- mapM tranObservation bodies'
  headsVals       <- mapM (mapM tranObservation) heads'
  let bodiesValue =  foldr (.&&.) true bodiesVals -- TODO
  let headsValue' =  map (foldr (.&&.) true) headsVals -- TODO
  let headsValue  =  foldr (.||.) false headsValue' -- TODO
  assert $ bodiesValue .=>. headsValue
  where filterFunc b = case b of
                         Obs (FnRel _ [_]) -> False
                         otherwise         -> True

{- This function constructs a 'SatResult' instance, representing a model 
   returned by the SMT solver, by fetching values for the SMT names in the 
   container of the current computation (including element names in the domain,
   term names and atom names. -}
getSatResult :: MSMT SatResult
getSatResult = do
  res       <- checkSat
  if res
     then do
            container     <- lift State.get
            let domain     = Map.toList $ containerDomain container
            let terms      = Map.toList $ containerTerms container
            let atoms      = Map.toList $ containerAtoms container            
            domainVals    <- mapM (\(k, v) -> do
                                  v' <- getValue v
                                  return (k, Result RKInteger $ RVInteger v')) domain
            termsVals     <- mapM (\(k, v) -> do
                                  v' <- getValue v
                                  return (k, Result RKInteger $ RVInteger v')) terms
            atomsVals     <- mapM (\(k, v) -> do
                                  v' <- getValue v
                                  return (k, Result RKBool $ RVBool v')) atoms
            return $ Satisfiable $ Map.unions [ Map.fromList domainVals
                                              , Map.fromList termsVals
                                              , Map.fromList atomsVals]
     else return Unsatisfiable

{- Given an input observation, creates a symbolic value of type SBool inside a 
   symbolic computation of type SMT. The result of this function is used by 
   'addObservationSequent' to build a constraint for the input of SMT solver. -}
tranObservation :: Observation -> MSMT SBool
tranObservation obs@(Obs (Rel "=" _)) = do
  let smtObs@(SMTEqObs e1 e2)   = smtObservation obs
  [v1, v2]                     <- mapM elementValue [e1, e2]
  return (v1 .==. v2)
tranObservation obs@(Obs atm@(Rel r ts)) = do
  let (SMTFactObs atom) = smtObservation obs
  let r'                = smtRelSym r
  unintRel  <- unintRelValue r' (length ts) 
               -- Not good! symbols must have their arities with themselves. 
               -- This is subject to refactoring. 
  let elms  =  smtElement <$> (\(Elem e) -> e) <$> ts
  vals      <- mapM elementValue elms
  val       <- atomValue r' atom unintRel vals
  return (val .==. true)
tranObservation obs@(Obs atm@(FnRel f ts))  = do
  let (SMTFnObs term elm) = smtObservation obs
                         -- smtObservation drops the last parameter for 
                         -- functions
  let elms  =  smtElement <$> (\(Elem e) -> e) <$> (init ts)
  unintFunc <- unintFnValue f (length elms)
  sIns      <- mapM elementValue elms
  sOut      <- elementValue elm
  val       <- termValue f term unintFunc sIns
  return (val .==. sOut)

{- Converts a solution created by the SMT solver to a set of 'Observation's. -}
translateSolution :: SatResult -> Maybe Model
translateSolution  res = case res of
                           Unsatisfiable   -> Nothing
                           Satisfiable dic -> Just $ translateDictionary dic

{- A helper for 'translateSolution' -}
translateDictionary :: SatDictionary -> Model
translateDictionary dic =
    let (_, obss)    = Map.partitionWithKey (\k _ -> isElementString k) dic
        (rels, funs) = Map.partition (\cw -> resKind cw == RKBool) obss
        eqClasses    = equivalenceClasses dic
        revDic       = Map.map head eqClasses
        relObss      = Map.elems $ Map.mapWithKey (\k _ -> obsFromSMTAtom k) 
                       (Map.filter (\cw -> resValue cw == RVBool True) rels)
        funObss      = Map.elems $ Map.mapWithKey 
                       (\k cw -> (obsFromSMTTerm k) <$> (Map.lookup cw revDic))
                       funs
        funObss'     = fromJust <$> filter isJust funObss
        domain       = let lists = Map.elems eqClasses
                           pairs = (\l -> (head l, l)) 
                                   <$> ((readElement <$>) <$> lists)
                       in  Map.fromList pairs
    in  createModel domain $ relObss `union` funObss'


{- Given an 'SMTAtom' for a relational fact, creates an observation. In some
   sense, this funciton works in the opposite direction of 'smtAtom'. -}
obsFromSMTAtom :: SMTAtom -> Observation
obsFromSMTAtom str = 
    let strs = Text.unpack <$> Text.splitOn (Text.pack "-") (Text.pack str)
        sym  = head strs
        es   = tail strs
        sym' = relSymFromSMT sym
    in  Obs $ Rel sym' $ (Elem . readElement) <$> es

{- Similar to 'obsFromSMTAtom', but constructs an 'Observatin' for a functional 
   term.  -}
obsFromSMTTerm :: SMTTerm -> SMTElement -> Observation
obsFromSMTTerm str e = 
    let strs = Text.unpack <$> Text.splitOn (Text.pack "-") (Text.pack str)
        sym  = head strs
        es   = tail strs
    in  Obs $ FnRel sym $ (Elem . readElement) <$> (es ++ [e])

{- This function constructs equivalence classes on elements of the domain, 
   (represented by 'SMTElement' instances) based on the the result of 
   SMT solving. The input is simply a map from 'SMTElement's to the values that
   they have in the SMT result and the output is their equivalence classes 
   together with their values in the SMT result. 
   This function is useful when
     a. translating the solution to a set of 'Observation's
     b. constructing additional constraints for reducing an initial result from
     the SMT solver to a minimal result. -}
equivalenceClasses :: Map.Map SMTElement Result -> Map.Map Result [SMTElement]
equivalenceClasses dic = 
    let elems    = Map.filterWithKey (\k _ -> isElementString k) dic
        list     = sortBy (\(x, y) (x', y') -> 
                               case compare y y' of
                                 EQ -> compare x x'
                                 c  -> c) $ Map.toList elems
        classes  = groupBy (\(_, x) (_, y) -> x == y) list
    in Map.fromList $ (\c -> (snd (head c), fst <$> c)) <$> classes

--------------------------------------------------------------------------------
-- SMTLib2 Implementation
--------------------------------------------------------------------------------
-- Every element, term and atomic formula must be assigned to a value of an
-- input type, supported by SMTLib2. We assign elements (represented by type
-- SMTElement) and terms (of type SMTTerm) to values of type SElement. 

{- SBool is an 'SMTExpr' expression of type Bool. 
   REMARK: SBool is the type of Symbolic boolean values in the SBV platform. We
   use this term to maintain some consistency with the SBV implementation of
   our SMT solving solution. -}
type SBool = SMTExpr Bool

{- Values of elements -}
type SElement = SMTExpr Integer

{- Symbolic domain is a map from 'SMTElement' to their values of type SElement. 
-}
type SDomain = Map.Map SMTElement SElement

{- STerms is a map from 'SMTTerm' to values of type 'SElement' -}
type STerms  = Map.Map SMTTerm SElement

{- SAtoms is a map from 'SMTAtom' to values of type 'SBool' -}
type SAtoms  = Map.Map SMTAtom SBool

{- Some constant values 
   REMARK: Again, the constant values @true@ and @false@ are compatible with the 
   constants defined by SBV. -}
true :: SMTExpr Bool
true = constant True

false :: SMTExpr Bool
false = constant False

{- UninterpretFn presents SMTLib2 functions that are used to construct the input
   query to the SMT solver. Because these functions apply on values of Haskell 
   types, the arity of their inputs must be fixed. SMTLib2 represents the inputs 
   of a function of an arbitrary arity using tuples. -}
data UninterpretFn =
       UnintFn0 SElement -- Functions of arity 0 are constants
     | UnintFn1 (SMTFunction SElement Integer)
     | UnintFn2 (SMTFunction (SElement, SElement) Integer)
     | UnintFn3 (SMTFunction (SElement, SElement, SElement) Integer)
--      | UnintFn4 (SMTFunction (SElement, SElement, SElement, SElement) Integer)
--      | UnintFn5 (SMTFunction (SElement, SElement, SElement, SElement, SElement)
--                              Integer)
--      | UnintFn6 (SMTFunction (SElement, SElement, SElement, SElement, SElement
--                              , SElement) Integer)
--      | UnintFn7 (SMTFunction (SElement, SElement, SElement, SElement, SElement
--                              , SElement, SElement) Integer)

{- Show instance for 'UninterpretFn' -}
instance Show UninterpretFn where
    show (UnintFn0 _) = "UnintFn0"
    show (UnintFn1 _) = "UnintFn1"
    show (UnintFn2 _) = "UnintFn2"
    show (UnintFn3 _) = "UnintFn3"
    -- show (UnintFn4 _) = "UnintFn4"
    -- show (UnintFn5 _) = "UnintFn5"
    -- show (UnintFn6 _) = "UnintFn6"
    -- show (UnintFn7 _) = "UnintFn7"

{- Creating 'UninterpretFn' for a given arity -}
-- TODO: do we need the first parameter here?
uninterpretFn :: FnSym -> Int -> MSMT UninterpretFn
uninterpretFn fn 0 = (liftM UnintFn0) var -- TODO: is this correct?
uninterpretFn fn 1 = (liftM UnintFn1) fun
uninterpretFn fn 2 = (liftM UnintFn2) fun
uninterpretFn fn 3 = (liftM UnintFn3) fun
-- uninterpretFn fn 4 = (liftM UnintFn4) fun
-- uninterpretFn fn 5 = (liftM UnintFn5) fun
-- uninterpretFn fn 6 = (liftM UnintFn6) fun
-- uninterpretFn fn 7 = (liftM UnintFn7) fun

{- Returns the arity of a given 'UninterpretFn' -}
unintFnArity :: UninterpretFn -> Int
unintFnArity (UnintFn0 _) = 0
unintFnArity (UnintFn1 _) = 1
unintFnArity (UnintFn2 _) = 2
unintFnArity (UnintFn3 _) = 3
-- unintFnArity (UnintFn4 _) = 4
-- unintFnArity (UnintFn5 _) = 5
-- unintFnArity (UnintFn6 _) = 6
-- unintFnArity (UnintFn7 _) = 7

{- Applies an instance of 'UninterpretFn' to a list of symbolic arguments -}
applyUnintFn :: UninterpretFn -> [SElement] -> SElement
applyUnintFn (UnintFn0 f) []
    = f -- constant
applyUnintFn (UnintFn1 f) [x1] 
    = f `app` x1
applyUnintFn (UnintFn2 f) [x1, x2] 
    = f `app` (x1, x2)
applyUnintFn (UnintFn3 f) [x1, x2, x3] 
    = f `app` (x1, x2, x3)
-- applyUnintFn (UnintFn4 f) [x1, x2, x3, x4] 
--     = f `app` (x1, x2, x3, x4)
-- applyUnintFn (UnintFn5 f) [x1, x2, x3, x4, x5]
--     = f `app` (x1, x2, x3, x4, x5)
-- applyUnintFn (UnintFn6 f) [x1, x2, x3, x4, x5, x6] 
--     = f `app` (x1, x2, x3, x4, x5, x6)
-- applyUnintFn (UnintFn7 f) [x1, x2, x3, x4, x5, x6, x7] 
--     = f `app` (x1, x2, x3, x4, x5, x6, x7)
-- applyUnintFn _ _ = error $ unitName ++ ".applyUnintFn: " ++ error_InvalidFnArity

{- UninterpretRel presents symbolic relations that are used in SBV for creating 
   the input query to the SMT solver. Because these relations are Haskell 
   functions that operate on symbolic values, their types must be fixed; 
   that is, we cannot have relations of arbitrary arities. -}
data UninterpretRel = UnintRel0 SBool
                    | UnintRel1 (SMTFunction SElement Bool)
                    | UnintRel2 (SMTFunction (SElement, SElement) Bool)
                    | UnintRel3 (SMTFunction (SElement, SElement, SElement)
                                Bool)
                    -- | UnintRel4 (SMTFunction (SElement, SElement, SElement
                    --             , SElement) Bool)
                    -- | UnintRel5 (SMTFunction (SElement, SElement, SElement
                    --             , SElement, SElement) Bool)
                    -- | UnintRel6 (SMTFunction (SElement, SElement, SElement
                    --             , SElement, SElement, SElement) Bool)
                    -- | UnintRel7 (SMTFunction (SElement, SElement, SElement
                    --             , SElement, SElement, SElement, SElement) Bool)

{- Show instance for 'UninterpretRel' -}
instance Show UninterpretRel where
    show (UnintRel0 _) = "UnintRel0"
    show (UnintRel1 _) = "UnintRel1"
    show (UnintRel2 _) = "UnintRel2"
    show (UnintRel3 _) = "UnintRel3"
--     show (UnintRel4 _) = "UnintRel4"
--     show (UnintRel5 _) = "UnintRel5"
--     show (UnintRel6 _) = "UnintRel6"
--     show (UnintRel7 _) = "UnintRel7"

{- Creating 'UninterpretRel' for a given arity -}
uninterpretRel :: RelSym -> Int -> MSMT UninterpretRel
uninterpretRel rel 0 = (liftM UnintRel0) var
uninterpretRel rel 1 = (liftM UnintRel1) fun
uninterpretRel rel 2 = (liftM UnintRel2) fun
uninterpretRel rel 3 = (liftM UnintRel3) fun
-- uninterpretRel rel 4 = UnintRel4 (uninterpret rel)
-- uninterpretRel rel 5 = UnintRel5 (uninterpret rel)
-- uninterpretRel rel 6 = UnintRel6 (uninterpret rel)
-- uninterpretRel rel 7 = UnintRel7 (uninterpret rel)

{- Returns the arity of a given 'UninterpretRel' -}
unintRelArity :: UninterpretRel -> Int
unintRelArity (UnintRel0 _) = 0
unintRelArity (UnintRel1 _) = 1
unintRelArity (UnintRel2 _) = 2
unintRelArity (UnintRel3 _) = 3
-- unintRelArity (UnintRel4 _) = 4
-- unintRelArity (UnintRel5 _) = 5
-- unintRelArity (UnintRel6 _) = 6
-- unintRelArity (UnintRel7 _) = 7

{- Applies an instance of 'UninterpretRel' to a list of symbolic arguments. -}
applyUnintRel :: UninterpretRel -> [SElement] -> SBool
applyUnintRel (UnintRel0 r) []
    = r
applyUnintRel (UnintRel1 r) [x1] 
    = r `app` x1
applyUnintRel (UnintRel2 r) [x1, x2] 
    = r `app` (x1, x2)
applyUnintRel (UnintRel3 r) [x1, x2, x3] 
    = r `app` (x1, x2, x3)
-- applyUnintRel (UnintRel4 r) [x1, x2, x3, x4] 
--     = r `app` (x1, x2, x3, x4)
-- applyUnintRel (UnintRel5 r) [x1, x2, x3, x4, x5]
--     = f `app` (x1, x2, x3, x4, x5)
-- applyUnintRel (UnintRel6 r) [x1, x2, x3, x4, x5, x6] 
--     = f `app` (x1, x2, x3, x4, x5, x6)
-- applyUnintRel (UnintRel7 r) [x1, x2, x3, x4, x5, x6, x7] 
--     = f `app` (x1, x2, x3, x4, x5, x6, x7)
-- applyUnintRel _ _ = error $ unitName ++ ".applyUnintRel: " 
--                     ++ error_InvalidRelArity

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
    , containerFns    :: Map.Map FnSym UninterpretFn
    , containerRels   :: Map.Map RelSym UninterpretRel
    , containerTerms  :: STerms
    , containerAtoms  :: SAtoms 
    }

{- Initial empty container -}
emptySMTContainer :: SMTContainer 
emptySMTContainer  = SMTContainer Map.empty Map.empty Map.empty Map.empty Map.empty

{- 'MSMT' is the computation context for translation to MSMT and storing SMT 
   queries, a context for sending 'Observation's to SMT values and running the
   SMT solver. -}
type MSMT = SMT' (State.StateT SMTContainer IO)

{- Returns an SMTLib2 expression for an element name of type 'SMTElement' inside
   the MSMT computation. The symbolic value is fetched from the 'SMTContaiener'
   inside the computation or will be created (and inserted into the container)
   if it doesn't exist. -} 
elementValue :: SMTElement -> MSMT SElement
elementValue elm = do
  container <- lift State.get
  let domMap = containerDomain container
  sym       <- case Map.lookup elm domMap of
                 Nothing -> var
                 Just s  -> return s
  let domMap' = Map.insertWith (flip const) elm sym domMap
  lift $ State.modify (\c -> c { containerDomain = domMap' })
  return sym

{- Returns an SMTLib2 expression for the input relation symbol of the given 
   arity inside the MSMT computation. This creates a new UninterpretRel value if
   it already does not exist in the container of computation.-}
unintRelValue :: RelSym -> Int -> MSMT UninterpretRel
unintRelValue rel arity = do
  container   <- lift State.get
  let relMap   = containerRels container
  relExp      <- uninterpretRel rel arity
  let unintRel = Map.findWithDefault relExp rel relMap
  -- update container:
  let relMap'  = Map.insertWith (flip const) rel unintRel relMap
                 -- do not insert if exists
  lift $ State.modify (\c -> c { containerRels = relMap' })
  return unintRel

{- Returns an SMTLib2 expression for the input function symbol of the given 
   arity inside the MSMT computation. This creates a new UninterpretFn value if
   it already does not exist in the container of computation.-}
unintFnValue :: FnSym -> Int -> MSMT UninterpretFn
unintFnValue fn arity = do
  container    <- lift State.get
  let fnMap     = containerFns container
  fnExp        <- uninterpretFn fn arity
  let unintFunc = Map.findWithDefault fnExp fn fnMap
  -- update container:
  let fnMap'    = Map.insertWith (flip const) fn unintFunc fnMap
                 -- do not insert if exists
  lift $ State.modify (\c -> c { containerFns = fnMap' })
  return unintFunc

{- For an atomic relation of type 'SMTAtom', returns a boolean SMTLib2 expression 
   inside the MSMT computation. If such value does not exists, it will apply the
   given uninterpreted function to the given element expressions to create the
   value and add it to the container of computation. -}
atomValue :: RelSym -> SMTAtom -> UninterpretRel -> [SElement] -> MSMT SBool
atomValue rel term unintRel sParams = do
  container  <- lift State.get
  let atomMap =  containerAtoms container
  sym        <- case Map.lookup term atomMap of
                  Nothing -> do
                    s <- var
                    assert $ (applyUnintRel unintRel sParams) .==. s
                    return s
                  Just s  -> return s
  -- update container:
  let atomMap' = Map.insertWith (flip const) term sym atomMap
                 -- do not insert if exists
  lift $ State.modify (\c -> c { containerAtoms = atomMap' })
  return sym

{- For an functional term of type 'SMTTerm', returns a value of type 'SElement'
   inside an MSMT computation. If such value does not exists, it will apply the
   given uninterpreted function to the given element expressions to create the
   value and add it to the container of computation. -}
termValue :: FnSym -> SMTTerm -> UninterpretFn -> [SElement] -> MSMT SElement
termValue fn term unintFunc sParams = do
  container   <- lift State.get
  let termMap  = containerTerms container
  sym         <- case Map.lookup term termMap of
                   Nothing -> do
                     s <- var
                     assert $ (applyUnintFn unintFunc sParams) .==. s
                     return s
                   Just s  -> return s
  -- update container:
  let termMap' = Map.insertWith (flip const) term sym termMap 
                 -- do not insert if exists
  lift $ State.modify (\c -> c { containerTerms = termMap' })
  return sym

--------------------------------------------------------------------------------
-- SMT Solving and Model Generation
--------------------------------------------------------------------------------
{- Defining a SATSolver instance that does SMT solving. The 'SMTAtom' type of 
   this implementation is SMTObservation and the type of SMT solving data is 
   MSMT (). -}
instance SATSolver SMTObservation (MSMT ()) where
    satInitialize (SMTTheory context)      = context
    satSolve context = let (res, context') = minimumResult context
                       in  (translateSolution res, context')

{- Given a query of type MSMT, executes the query and returns the result. It 
   also returns a new 'MSMT' computation where the homomorphism cone of the 
   minimum result is eliminated. -}
minimumResult :: MSMT () -> (SatResult, MSMT ())
minimumResult context = 
    let context'   = do
          context
          getSatResult
        run        = withZ3 context'
        res        = unsafePerformIO $ State.evalStateT run emptySMTContainer
        minRes     = fst $ reduce context res
        newContext = do
          context
          constraint <- nextResult minRes
          assert constraint
    in  (minRes, newContext)

{- This recursively reduces the initial result of the SMT solver to construct a 
   minimal model based on an Aluminum-like algorithm. The result of the function
   is the minimal result as well as a new computation context that contains the
   additional constraints for reducing the input result. -}
reduce :: MSMT() -> SatResult -> (SatResult, MSMT ())
reduce context res@(Satisfiable _) = 
    let context'  = do
          context
          constraint <- minimizeResult res
          assert constraint
        run        = withZ3 (do
                              context'
                              getSatResult)
        res'       = unsafePerformIO $ State.evalStateT run emptySMTContainer
    in  case res' of
          Unsatisfiable -> (res, context')
          Satisfiable _ -> reduce context' res'
reduce context res = (res, context)

{- Constructs the a set of constraints for minimizing the result returned by the
   SMT solver and returns their corresponding value of type SBool in the context
   of the MSMT computation. These constraints are four-fold:
   
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
minimizeResult :: SatResult -> MSMT SBool
minimizeResult res = do
  container   <- lift State.get
  let domain   = containerDomain container
  let rels     = Map.toList $ containerRels container
  let dic      = getSatResultDictionary res
  let classes  = equivalenceClasses dic
  let sClasses = ((\e -> fromJust $ Map.lookup e domain) <$>) 
                 <$> (Map.elems classes)

  let (factDic, othDic ) = Map.partition (\s -> resKind s == RKBool) dic
      -- Elements of SatResult that are of type KBool correspond to facts
  let (posDic, negDic)   = Map.partition (\s -> resValue s == RVBool True) factDic
      -- Partitioning factDic into positive and negative facts

  let posFacts = Map.keys posDic -- Positive Facts
  let negFacts = Map.keys negDic -- Negative Facts

  -- Creating maps from relation names to symAtoms in posFacts and negFacts:
  let posAtoms = Map.toList $ smtSymAtomMap posFacts
  let negAtoms = Map.toList $ smtSymAtomMap negFacts

  let termStrs = Map.toList 
                 $ Map.filterWithKey (\k _ -> not (isElementString k)) othDic
      -- The keys in the map that correspond to terms have type other than
      -- KBool (coming from othDic) and they are not element names.

  let eqNegAx  = equalityNegativeAxioms sClasses
  let eqFlipAx = foldr (.||.) false $ equalityFlipAxioms <$> sClasses -- TODO
  negs  <- mapM (\(r, unintRel) -> 
                     case lookup r negAtoms of
                       Nothing -> return true
                       Just as -> negativeAxioms unintRel as) rels
  fnNeg <- functionNegativeAxioms termStrs classes
  fnFlips <- functionFlipAxioms termStrs classes
  flips <- mapM (\(r, unintRel) -> 
                     case lookup r posAtoms of
                       Nothing -> return false
                       Just as -> flipAxioms unintRel as) rels
  let negAx    = foldr (.&&.) true negs -- TODO
  let flipAx   = foldr (.||.) false flips -- TODO
  return $ negAx .&&. fnNeg .&&. eqNegAx .&&. (flipAx .||. fnFlips .||. eqFlipAx)

{- This function creates Negative Axioms for a set of facts that are named by
   a list of SMTAtom instances: every SMTAtom names a *negative* fact in a 
   result from the SMT solver. -}
negativeAxioms :: UninterpretRel -> [SMTAtom] -> MSMT SBool
negativeAxioms unintRel atoms = do
  container   <- lift State.get
  let domain   = containerDomain container
  let arity    = unintRelArity unintRel
  let argNames = atomArgs <$> atoms
  let args     = ((\a -> fromJust $ Map.lookup a domain) <$>) <$> argNames
  return $ foldr (.&&.) true $ (\t -> not' $ applyUnintRel unintRel t) <$> args -- TODO

functionNegativeAxioms :: [(SMTTerm, Result)] -> Map.Map Result [SMTElement]
                       -> MSMT SBool
functionNegativeAxioms terms classes = do
  container    <- lift State.get
  let domain    = containerDomain container
  let contTerms = containerTerms container
  let classes'  = 
          Map.map (\es -> (\e -> fromJust $ Map.lookup e domain) <$> es) classes
  let content   = (\(t, cw) -> ( fromJust $ Map.lookup t contTerms
                               , otherClasses cw classes')) <$> terms
  let results   = (uncurry functionNegativeAxiomsHelper) <$> content
  return $ foldr (.&&.) true results -- TODO
    where otherClasses c cs = Map.elems $ Map.delete c cs

functionNegativeAxiomsHelper :: SElement -> [[SElement]] -> SBool
functionNegativeAxiomsHelper term classes = do
  let eqs   = concatMap ((\e -> not' $ e .==. term) <$>) classes -- TODO
  foldr (.&&.) true eqs -- TODO

functionFlipAxioms :: [(SMTTerm, Result)] -> Map.Map Result [SMTElement]
                   -> MSMT SBool
functionFlipAxioms terms classes = do
  container    <- lift State.get
  let domain    = containerDomain container
  let contTerms = containerTerms container
  let classes'  = 
          Map.map (\es -> (\e -> fromJust $ 
                                 Map.lookup e domain) <$> es) classes
  let content   = (\(t, cw) -> ( fromJust $ 
                                 Map.lookup t contTerms
                               , thisClass cw classes')) <$> terms
  let results   = (uncurry functionFlipAxiomsHelper) <$> content
  return $ foldr (.||.) false results -- TODO
    where thisClass c cs = Map.findWithDefault [] c cs
  
functionFlipAxiomsHelper :: SElement -> [SElement] -> SBool
functionFlipAxiomsHelper term cls = do
  let eqs   = (\e -> not' $ e .==. term) <$> cls -- TODO
  foldr (.||.) false eqs -- TODO

{- This function creates Flip Axioms for a set of facts that are named by a list
   of SMTAtom instances. Just like 'negativeAxioms', the function assumes that
   all the SMTAtom instances belong to the same relation whose symbolic 
   uninterpreted function is given to the function. -}
flipAxioms :: UninterpretRel -> [SMTAtom] -> MSMT SBool
flipAxioms unintRel atoms = do
  container   <- lift State.get
  let domain    = containerDomain container
  let argNames  = map atomArgs atoms      
  let allArgs   = map (\elm -> fromJust $ Map.lookup elm domain) <$> argNames
  let negApps   = not'.(applyUnintRel unintRel) <$> allArgs
  return $ foldr (.||.) false negApps -- TODO

{- Given a set of equivalence classes on all elements, creates the set of 
   Equality Negative Axioms. The function recursively makes sure that the 
   elements of an equivalence class will never be equal to the elements of 
   other equivalence class. -}
equalityNegativeAxioms :: [[SElement]] -> SBool
equalityNegativeAxioms []            = true
equalityNegativeAxioms (cls:classes) =
    let this = foldr (.&&.) true [differentClasses cls cls' | cls' <- classes] -- TODO
    in  this .&&. equalityNegativeAxioms classes

{- As a helper for 'equalityNegativeAxioms', creates a constraint for two sets 
   of elements in different equivalence classes that prevents them from being
   equal after minimization. -}
differentClasses :: [SElement] -> [SElement] -> SBool
differentClasses cls1 cls2 =
    foldr (.&&.) true [not' $ e1 .==. e2 | e1 <- cls1, e2 <- cls2] -- TODO

{- Given an equivalence class of elements, creates an axiom to test whether any 
   of the two elements in the class may be unequal after minimization or not. -}
equalityFlipAxioms :: [SElement] -> SBool
equalityFlipAxioms []     = false
equalityFlipAxioms (e:es) = 
    let this = foldr (.||.) false [not' $ e .==. e' | e' <- es] -- TODO
    in  this .||. equalityFlipAxioms es


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
nextResult :: SatResult -> MSMT SBool
nextResult res = do
  container   <- lift State.get
  let domain   = containerDomain container
  let rels     = Map.toList $ containerRels container
  let dic      = getSatResultDictionary res
  let termStrs = Map.toList $ Map.filterWithKey
                 (\k cw -> resKind cw /= RKBool && not (isElementString k)) dic
  let classes  = equivalenceClasses dic
  let sClasses = ((\e -> fromJust $ Map.lookup e domain) <$>) 
                 <$> (Map.elems classes)
  let factStrs = Map.keys $ Map.filter (\s -> resValue s == RVBool True ) dic
  let symAtoms = Map.toList $ smtSymAtomMap factStrs
  let eqFlipAx = foldr (.||.) false $ equalityFlipAxioms <$> sClasses -- TODO
  flips <- mapM (\(r, unintRel) -> 
                     let as = fromMaybe [] (lookup r symAtoms)
                     in  flipAxioms unintRel as) rels
  let flipAx   = foldr (.||.) false flips -- TODO
  funFlipAx   <- functionFlipAxioms termStrs classes
  return $ flipAx .||. eqFlipAx .||. funFlipAx

{- Given the name of an atomic fact as an instance of type 'SMTAtom', returns 
   the names of the symbolic values of the arguments. -}
atomArgs :: SMTAtom -> [SMTElement]
atomArgs  smtAtom = 
    tail $ Text.unpack <$> 
           Text.splitOn (Text.pack "-") (Text.pack smtAtom)


{- Does the name of a symbolic value correspond to an element of the model? -}
isElementString :: String -> Bool
isElementString ('e':'^':_) = True
isElementString _           = False