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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}

module API.Core where

-- Standard
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
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
import Common.IProvenance
import Common.Observation
import Common.Model (Model (..))
import Common.Input

-- Chase
import Chase.Data
import Chase.Impl
import qualified Chase.Chase

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
            (\cfg -> return cfg { configRelaxMin = True }))
        "Allow models that are not purely minimal"        
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
        (NoArg
          (\_ -> do
            prg <- getProgName
            hPutStrLn stderr (usageInfo prg options)
            exitWith ExitSuccess))
        "Show help"
    ]
parseConfig :: [String] -> IO Config
parseConfig args = do
  -- Parse options, getting a list of option actions
  let (actions, nonOptions, errors) = getOpt Permute options args
  -- Here we thread startOptions through all supplied option actions
  config      <- foldl (>>=) (return defaultConfig) actions
  let config'  = case configInput config of  -- if the input file not specified
                   Nothing -> if   null nonOptions
                              then config
                              else config {configInput = Just $ head nonOptions}
                                   -- use the first nonOption for the input file
                   Just _  -> config
  return config'
-- In: configuration options, user input theory
-- Out: an input if parsing success
parseInputFile :: Config -> String -> Either String Input
parseInputFile config input = parseInput input

---------------------
-- Chase Data / G* --
---------------------
-- In: configuration, theory
-- Out: G*, which consists of ground facts, provenance info, and a propositional theory
generateChase :: Config -> Theory -> ( ChasePossibleFactsType, ProvInfo
                                     , SATIteratorType, Int)
generateChase config theory = chase config theory
-- In: G*, new observation
-- Out: an augmented G* with the new observation
augmentChase :: Config -> Theory -> (ChasePossibleFactsType, ProvInfo,  SATIteratorType, Int)
             -> (Observation,[Element]) -> (ChasePossibleFactsType, ProvInfo, SATIteratorType, Int)
augmentChase = undefined
-- augmentChase cfg thy (b, p, it, c) (obs@(Obs (Rel rsym terms)),newelms) = do
--   let thy' = preprocess thy
--   let seqMap = (buildSequentMap $ fromJust <$> fromSequent <$> thy') :: SequentMap ChaseSequentType
--   -- update provenance
--   let ep' = foldr (\e->insertProv e (Fn "user" [Fn rsym terms])) (elementProvs p) newelms
--   let op' = Map.insert obs (UserBlame obs) (observationProvs p)
--   let p' = ProvInfo ep' op' 
--   -- update prop theory
--   let newSeq = ObservationSequent [] [[obs]]
--   let it'    = satStore (UserBlame obs, newSeq) it
--   -- return new GS
--   augmentBase cfg seqMap (b, p', it', c) obs
  
--
--
augmentBase :: Config -> SequentMap ChaseSequentType ->
            (ChasePossibleFactsType, ProvInfo, SATIteratorType, Int) -> Observation ->
            (ChasePossibleFactsType, ProvInfo, SATIteratorType, Int)
augmentBase = undefined
-- augmentBase _ _ gs eqobs@(Obs (Rel "=" terms)) = gs
-- augmentBase cfg seqMap (b, p, t, it, c) obs@(Obs (Rel rsym terms)) = do
--   let d = addToBase obs emptyBase
--   let seqMap' = Map.filter (not.startSequent) seqMap
--       -- The current implementation of the Chase instantiate existential
--       -- quantifiers even if they are already witnessed by an element in the
--       -- model. We don't want to regenerate new elements by processing sequents
--       -- with empty body for a second time.
--       -- FIX: How should we systematically address this issue?
--   Chase.Chase.resumeChase cfg c seqMap' b d p t

--------------
-- SAT Data --
--------------
-- In: a propositional theory
-- Out: an iterator that can be used to sequentially generate models (model stream)
openSAT :: Config -> SATIteratorType
openSAT = satInitialize
--
--
closeSAT :: SATIteratorType -> ()
closeSAT = satClose
-- In: a model stream
-- Out: an updated model stream and the next model
nextModel :: SATIteratorType -> (Maybe Model, SATIteratorType)
nextModel = satNext
--
--
upModel :: SATIteratorType -> (Maybe Model, SATIteratorType)
upModel = satAugment

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
        let blames = catMaybes $ map (\rule->getElementBlame rule mdl skolemtree) irules
        return ((head blames), r)
--
--
getElementBlame :: (Id, Sequent, Sequent) -> Model -> (Element, FnSym, [Element]) -> Maybe Blame
getElementBlame (rid, (Sequent bd' hd'), (Sequent bd hd)) mdl (e, f, r) = do
  let exists = formulaExistentials hd'
  let ruleskolems = map getskolemnames exists
  case elem f ruleskolems of
    False -> Nothing
    True -> do
      let freevars = freeVars bd
      let freefns = freeVars bd' \\ freevars
      let terms = map (\e->(Elem e)) r
      let (varterms, fnterms) = splitAt (length freevars) terms
      Just $ TheoryBlame rid $ Map.fromList $ (zip freevars varterms) ++ (zip freefns fnterms)
  where
    getskolemnames e = case e of
      Left fn -> fn
      Right (fn, _) -> fn
--
--
getObservationBlame :: ObservationProvs -> Model -> Observation -> Maybe Blame
getObservationBlame prov mdl obv@(Obs (Rel sym ts)) = do
  let eqelms = (map (getEqualElements mdl) ts)
  let possibilities = combination eqelms
  let obvs = map (\ts->(Obs (Rel sym (map(\t->(Elem t))ts)))) possibilities
  let blames = catMaybes $ map (\o->(Map.lookup o prov)) obvs
  case blames of
    [] -> Nothing
    blame:bs -> Just blame
getObservationBlame _ _ _ = Nothing
--
--
combination :: [[a]] -> [[a]]
combination [l] = do
  choose <- l
  return (return choose)
combination (l:ls) = do
  choose <- l
  rest <- (combination ls)
  return (choose : rest)
combination [] = []

-------------------
-- OTHER HELPERS --
-------------------
-- In: model of a theory, a term representing an element
-- Out: a list of elements this model is equivalent to in the given model
getEqualElements :: Model -> Term -> [Element]
getEqualElements mdl term = case term of
  (Elem e) -> case (Map.lookup e (modelElements mdl)) of
    Nothing -> []
    Just eqelms -> eqelms
  _ -> []
--
--
getSkolemHead :: Term -> Maybe (Either (FnSym, [Term]) Observation)
getSkolemHead skolemtree = do
  case skolemtree of
    (Fn "user" [(Fn rsym terms)]) -> Just $ Right (Obs (Rel rsym terms))
    (Fn skolemhead skolemrest) -> Just $ Left (skolemhead, skolemrest)
    (Cons (Constant skolemhead)) -> Just $ Left (skolemhead, [])
    _ -> Nothing
--
--
getSkolemElement :: ElementProvs -> Term -> Maybe Element
getSkolemElement prov skolemterm = (findElementWithProv skolemterm prov)
--
--
getSkolemTree :: ElementProvs -> Model -> Element -> Maybe (Either (Element, FnSym, [Element]) Blame)
getSkolemTree prov mdl elm = case (getElementProv elm prov) of
  Nothing   -> Nothing
  Just t -> case (getSkolemHead t) of
    Nothing -> Nothing
    Just tree -> case tree of
      Left (skolemhead, skolemrest) -> Just $ Left (elm, skolemhead, (concatMap (\t->(maybeToList (getSkolemElement prov t))) skolemrest))
      Right obs -> Just $ Right (UserBlame obs)
--
--
formulaElements :: Formula -> [Element]
formulaElements Tru            = []
formulaElements Fls            = []
formulaElements (And f1 f2)    = (formulaElements f1) `union` (formulaElements f2)
formulaElements (Or  f1 f2)    = (formulaElements f1) `union` (formulaElements f2)
formulaElements (Atm a)        = atomElements a
formulaElements (Exists _ x f) = (formulaElements f) 
--
--
atomElements :: Atom -> [Element]
atomElements (Rel   _ args)   = catMaybes $ termToElement <$> args
atomElements (FnRel _ args)   = catMaybes $ termToElement <$> args
--
--
sequentRename :: [Element] -> Sequent -> Sequent
sequentRename eqelms (Sequent bd hd) = Sequent (formulaRename bd eqelms) (formulaRename hd eqelms)
--
--
formulaRename :: Formula -> [Element] -> Formula
formulaRename (And f1 f2) eqelms    = And (formulaRename f1 eqelms) (formulaRename f2 eqelms)
formulaRename (Or  f1 f2) eqelms   = Or (formulaRename f1 eqelms) (formulaRename f2 eqelms)
formulaRename (Atm a) eqelms       = Atm $ atomRename a eqelms
formulaRename (Exists c x f) eqelms = Exists c x (formulaRename f eqelms) 
formulaRename fml _ = fml
--
--
atomRename :: Atom -> [Element] -> Atom
atomRename (Rel   sym args) eqelms  = Rel sym $ termRename eqelms <$> args 
atomRename (FnRel sym args) eqelms  = FnRel sym $ termRename eqelms <$> args
--
--
termRename :: [Element] -> Term -> Term
termRename eqelms term@(Elem e) = do
  if elem e eqelms
    then (Elem (head eqelms))
    else term
termRename _ term = term