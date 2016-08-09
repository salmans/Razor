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

  Module      : API.Core
  Description : The module provides the core API layer for interacting
  with the functional core of razor
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

{-# LANGUAGE FlexibleContexts, Rank2Types, LambdaCase #-}

module API.Core where

-- Standard
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import Safe
import System.Console.GetOpt
import System.Environment
import System.Exit (exitWith, ExitCode (..))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

-- Control
import Control.Applicative
import Control.Monad
import Control.Monad.Trans

-- Syntax
import Syntax.GeometricUtils
import Syntax.Term

-- Common
import Common.Data
import Common.Basic
import Common.Provenance
import Common.Observation
import Common.Model (Model (..))
import qualified Common.Input as CORE

-- Chase
import Chase.Data
import Chase.Impl

-- SAT
import SAT.Impl

-- Tools
import Tools.Config

------------
-- Config --
------------
-- In: command line args
-- Out: configuration options structure
options :: [ OptDescr (Config -> IO Config) ]
options =
    [ Option "i" ["input", "file"]
        (ReqArg
            (\arg cfg -> return cfg { configInput = Just arg })
            "FILE")
        "In theory file"
    , Option "d" ["debug"]
        (NoArg
            (\cfg -> return cfg { configDebug = True }))
        "Debug mode"
    , Option "s" ["depth"]
        (OptArg
            (\arg cfg -> return $ 
                         case join (readMaybe <$> arg) of
                           Nothing -> cfg
                           Just sk -> cfg { configDefaultSkolemDepth = sk})
            "#")
        "Depth of skolem term for reusing elements (-1 for pure minimal models)"
    , Option "r" ["relax"]
        (NoArg
            (\cfg -> return cfg { configPureMin = True }))
        "Allow models that are not purely minimal"   
    , Option "t" ["tptp", "tptp-path"]
        (ReqArg
            (\arg cfg -> return cfg {configTPTPPath = arg})
            "String")
        "TPTP Root Directory"     
    , Option "c" ["command", "cmd"]
        (ReqArg
            (\arg cfg -> return cfg { configCommand = Just arg })
            "String")
        "UserSyntax CLI Command"
    , Option "x" ["state"]
        (ReqArg
            (\arg cfg -> return cfg { configState = Just arg })
            "FILEPATH")
        "UserState represented as an XML file (program writes to this file)"
    , Option "h" ["help"]
        (NoArg . const $ do getProgName >>= hPutStrLn stderr . (`usageInfo` options)
                            exitWith ExitSuccess)
        "Show help"
    ]

parseConfig :: [String] -> IO Config
parseConfig args = do
  -- Parse options, getting a list of option actions
  let (actions, nonOptions, errors) = getOpt Permute options args
  -- Here we thread startOptions through all supplied option actions
  config <- foldl (>>=) (return defaultConfig) actions
  return $ case configInput config of  -- if the input file not specified
             Nothing -> if null nonOptions then config
                        else config {configInput = Just $ head nonOptions}
             -- use the first nonOption for the input file
             Just _  -> config

-- In: configuration options, user input theory
-- Out: an input if parsing success
-- XXX this ignores its Config parameter; why? [res]
parseInputFile :: Config -> String -> Either String CORE.Input
parseInputFile _ = CORE.parseInput

---------------------
-- Chase Data / G* --
---------------------
-- In: configuration, theory
-- Out: G*, which consists of ground facts, provenance info, and a propositional theory
generateChase :: Config -> Theory -> (ChasePossibleFactsType, ProvInfo, SATIteratorType, Int)
generateChase config theory = chase config theory
-- In: G*, new observation
-- Out: an augmented G* with the new observation
augmentChase :: Config -> Theory -> (ChasePossibleFactsType, ProvInfo,  SATIteratorType, Int) -> (Observation,[Element]) -> (ChasePossibleFactsType, ProvInfo, SATIteratorType, Int)
augmentChase cfg thy (b, p, it, c) (obs@(Obs (Rel rsym terms)),newelms) = do
  let newElmPreds = map (\e->Obs (elementPred (Elem e))) newelms
      obss = (obs:newElmPreds)
      newSeq = ObservationSequent [] [obss]
  -- update provenance
      ep' = foldr (\e->insertProv e (Fn "user" [Fn rsym terms])) (elementProvs p) newelms
      op' = Map.insert obs (UserBlame obs) (observationProvs p)
      bm' = Map.insert (UserBlame obs) newSeq (blameSequentMap p)
      p' = ProvInfo ep' op' bm'
  -- update SAT iterator
      it' = satStore newSeq $ satPush it
  -- rerun chase
      thy' = preprocess thy
      seqMap = (buildSequentMap $ fromJust <$> fromSequent <$> thy') :: SequentMap ChaseSequentType
  augmentBase cfg seqMap (b, p', it', c) obss

--
--
augmentBase :: Config -> SequentMap ChaseSequentType -> (ChasePossibleFactsType, ProvInfo, SATIteratorType, Int) -> [Observation] -> (ChasePossibleFactsType, ProvInfo, SATIteratorType, Int)
augmentBase cfg seqMap (b, p, it, c) obss@((Obs (Rel rsym terms)):rest) = do
  let d = foldr addToBase emptyBase obss
      seqMap' = Map.filter (not.startSequent) seqMap
     -- The current implementation of the Chase instantiate existential
     -- quantifiers even if they are already witnessed by an element in the
     -- model. We don't want to regenerate new elements by processing sequents
     -- with empty body for a second time.
     -- FIX: How should we systematically address this issue?
  resume cfg c seqMap' b d p it

--------------
-- SAT Data --
--------------
-- In: a model stream
-- Out: an updated model stream and the next model
nextModel :: SATIteratorType -> (Maybe Model, SATIteratorType)
nextModel it = do
  let it' = satPush it
  case satSolve it' of
    (Nothing, it'') -> (Nothing, oldit) where
      oldit = satPop it'
    els -> els
--
--
upModel :: SATIteratorType -> (Maybe Model, SATIteratorType)
upModel = satAugment
--
--
downModel :: SATIteratorType -> (Maybe Model, SATIteratorType)
downModel = satBacktrack . satPop
--
--
closeSAT :: SATIteratorType -> ()
closeSAT = satClose

----------------
-- PROVENANCE --
----------------
--
--
getBlamedSequent :: ProvInfo -> Blame -> Maybe Sequent
getBlamedSequent prov blame = case findBlameSequent blame prov of
  Nothing -> Nothing
  Just oseq -> Just $ toSequent oseq
--
--
getElementBlames :: Theory -> ElementProvs -> Model -> [Element] -> [(Blame, [Element])]
getElementBlames thy prov mdl eqelms = do
  let elm = head eqelms
  eqelm <- eqelms
  case getSkolemTree prov mdl eqelm of
    Nothing -> fail "no skolemtree"
    Just tree -> case tree of
      Right blame -> return (blame, [])
      Left skolemtree@(e, f, r) -> do
        let irules = map (\(r', r)->((fromMaybe 0 (elemIndex r' (preprocess thy)))+1, r', r)) $ zip (preprocess thy) thy
            blames = catMaybes $ map (\rule->getElementBlame rule mdl skolemtree) irules
        return ((head blames), r)
--
--
getElementBlame :: (Id, Sequent, Sequent) -> Model -> (Element, FnSym, [Element]) -> Maybe Blame
getElementBlame (rid, (Sequent bd' hd'), (Sequent bd hd)) mdl (e, f, r) = do
  let exists = formulaExistentials hd'
      ruleskolems = map getskolemnames exists
  case elem f ruleskolems of
    False -> Nothing
    True -> do
      let freevars = freeVars bd
          freefns = freeVars bd' \\ freevars
          terms = map Elem r
          (varterms, fnterms) = splitAt (length freevars) terms
      Just . TheoryBlame rid . Map.fromList $ (zip freevars varterms) ++ (zip freefns fnterms)
  where
    getskolemnames = \case
      Left fn       -> fn
      Right (fn, _) -> fn
--
--
getObservationBlame :: ObservationProvs -> Model -> Observation -> Maybe Blame
getObservationBlame prov mdl obv@(Obs (Rel sym ts)) =
  let eqelms = map (getEqualElements mdl) ts
      obvs   = map (Obs . Rel sym . map Elem) $ combination eqelms
      blames = catMaybes $ map (`Map.lookup` prov) obvs
  in headMay blames

getObservationBlame _ _ _ = Nothing

combination :: [[a]] -> [[a]]

combination [l] = do
  choose <- l
  return $ return choose

combination (l:ls) = do
  choose <- l
  rest <- combination ls
  return $ choose : rest

combination [] = []

-------------------
-- OTHER HELPERS --
-------------------
-- In: model of a theory, a term representing an element
-- Out: a list of elements this model is equivalent to in the given model

getEqualElements :: Model -> Term -> [Element]
getEqualElements mdl = \case
  (Elem e) -> maybe [] id . Map.lookup e $ modelElements mdl
  _        -> []

getSkolemHead :: Term -> Maybe (Either (FnSym, [Term]) Observation)
getSkolemHead = \case
  (Fn "user" [(Fn rsym terms)]) -> Just $ Right (Obs (Rel rsym terms))
  (Fn skolemhead skolemrest)    -> Just $ Left (skolemhead, skolemrest)
  (Cons (Constant skolemhead))  -> Just $ Left (skolemhead, [])
  _ -> Nothing

getSkolemTree :: ElementProvs -> Model -> Element -> Maybe (Either (Element, FnSym, [Element]) Blame)
getSkolemTree prov mdl elm =
  (\case Left (skolemhead, skolemrest) ->
           Left (elm, skolemhead, concatMap (maybeToList . (`findElementWithProv` prov)) skolemrest)
         Right obs ->
           Right (UserBlame obs))
  <$> (getElementProv elm prov >>= getSkolemHead)

formulaElements :: Formula -> [Element]
formulaElements = \case
  Tru            -> []
  Fls            -> []
  (And f1 f2)    -> combine f1 f2
  (Or  f1 f2)    -> combine f1 f2
  (Atm a)        -> atomElements a
  (Exists _ x f) -> formulaElements f

  where combine x y = formulaElements x `union` formulaElements y

atomElements :: Atom -> [Element]
atomElements = \case
  (Rel   _ args) -> result args
  (FnRel _ args) -> result args
  -- XXX refutable match
  where result = catMaybes . fmap termToElement

trueElementSequent :: Model -> ObservationSequent -> Sequent
trueElementSequent mdl oseq =
  let eqnames = Map.elems $ modelElements mdl
      renamed = foldr (liftA2 renameObservationSequent id head) oseq eqnames
  in toSequent renamed

termRename eqnames actualelm = \case
  term@(Elem e)       -> if elem e eqnames then Elem actualelm else term
  term@(Fn sym terms) -> Fn sym $ termRename eqnames actualelm <$> terms
  term                -> term

renameAtom :: [Element] -> Element -> Atom -> Atom
renameAtom eqnames actualelm = \case
  (Rel   rsym terms) -> Rel   rsym $ rename terms
  (FnRel fsym terms) -> FnRel fsym $ rename terms
  where rename = fmap $ termRename eqnames actualelm

renameObservation :: [Element] -> Element -> Observation -> Observation
renameObservation eqnames actualelm (Obs atm) = Obs $ renameAtom eqnames actualelm atm

renameConjuncts :: [Element] -> Element -> [Observation] -> [Observation]
renameConjuncts eqnames actualelm cs = filter (not.trivialObservation) $ renameObservation eqnames actualelm <$> cs

renameDisjuncts :: [Element] -> Element -> [[Observation]] -> [[Observation]]
renameDisjuncts eqnames actualelm ds = filter (not.trivialObservation) <$> renameConjuncts eqnames actualelm <$> ds

renameObservationSequent :: [Element] -> Element -> ObservationSequent -> ObservationSequent
renameObservationSequent eqnames actualelm (ObservationSequent body heads) =
  ObservationSequent (renameConjuncts eqnames actualelm body)
                     (renameDisjuncts eqnames actualelm heads)
