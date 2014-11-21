{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-|
  Razor
  Module      : API.Core
  Description : The module provides the core API layer for interacting with the functional core of razor
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module API.Core where
import Chase.Impl
import qualified Chase.Chase
import Chase.Data
import Common.Data
import Common.Basic
import Common.IProvenance
import Common.IObservation
import Common.Model (Model (..))
import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Map as Map
import Data.List
import qualified Data.Text as T
import SAT.Impl
import Syntax.GeometricUtils
import Syntax.Term
import System.Console.GetOpt
import System.Environment
import System.Exit (exitWith, ExitCode (..))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)
import Tools.Config
import Tools.Utils (isRealLine)
import Control.Monad.Trans

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
    , Option "s" ["skolem-depth"]
        (OptArg
            (\arg cfg -> return $ 
                         case join (readMaybe <$> arg) of
                           Nothing -> cfg
                           Just sk -> cfg { configSkolemDepth = sk})
            "#")
        "Depth of skolem term for reusing elements (-1 for pure minimal models)"
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
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  -- Here we thread startOptions through all supplied option actions
  config <- foldl (>>=) (return defaultConfig) actions
  return config
-- In: configuration options, user input theory
-- Out: a theory if parsing success
parseTheory :: Config -> String -> IO (Maybe Theory)
parseTheory config input = do
	let inputLines = lines input
	-- let sequents = mapM (parseFolToSequents False) (filter isRealLine inputLines)
	-- return $ concat <$> sequents
        let sequents = map parseSequent (filter isRealLine inputLines)
        return $ Just sequents
-- In: configuration, theory
-- Out: G*, which consists of ground facts, provenance info, and a propositional theory
generateGS :: Config -> Theory -> (ChaseHerbrandBaseType, ProvInfo, SATTheoryType)
generateGS config theory = chase config theory
-- In: G*, new observation
-- Out: an augmented G* with the new observation
augment :: Config -> Theory -> (ChaseHerbrandBaseType, ProvInfo, SATTheoryType) -> Observation -> (ChaseHerbrandBaseType, ProvInfo, SATTheoryType)
augment cfg thy (b, p, t) obs = do
  let thy' = preprocess thy
  let seqMap = (buildSequentMap $ fromSequent <$> thy') :: SequentMap ChaseSequentType
  let (b', p', t') = augmentGS cfg seqMap (b, p, t) obs
  let newSeq = ObservationSequent [] [[obs]]
  let t'' = storeSequent t' (UserBlame obs, newSeq)
  (b', p', t'')
--
--
augmentGS :: Config -> SequentMap ChaseSequentType -> (ChaseHerbrandBaseType, ProvInfo, SATTheoryType) -> Observation -> (ChaseHerbrandBaseType, ProvInfo, SATTheoryType)
augmentGS _ _ gs eqobs@(Obs (Rel "=" terms)) = gs
augmentGS cfg seqMap (b, p, t) obs@(Obs (Rel rsym terms)) = do
  let d = addToBase obs emptyBase
  Chase.Chase.resumeChase cfg seqMap b d p t
-- In: a propositional theory
-- Out: an iterator that can be used to sequentially generate models (model stream)
modelStream :: SATTheoryType -> SATIteratorType
modelStream propThy = satInitialize propThy
-- In: a model stream
-- Out: an updated model stream and the next model
nextModel :: SATIteratorType -> (Maybe Model, SATIteratorType)
nextModel it = (satSolve it)

----------------------
-- MODEL PROVENANCE --
----------------------
data ModelProv = ModelProv { nameProv :: NameProv, blameProv :: BlameProv }

type NameProv = Map.Map Element (TheorySub, [Element])
type BlameProv = Map.Map [Atom] TheorySub
type TheorySub = Map.Map Int RuleSub
data RuleSub = RuleSub { existSub :: ExistSub
                        ,freeSub :: FreeSub
                        ,funcSub :: FuncSub}
type FreeSub = Sub
type ExistSub = Map.Map (Int,Variable) Term
type FuncSub = Map.Map (FnSym, [Term]) Term
--
--
deriveModelProv :: Theory -> ProvInfo -> Model -> ModelProv
deriveModelProv thy prov mdl = ModelProv (deriveNameProvs thy (elementProvs prov) mdl) (deriveBlameProv thy prov mdl)
--
--
emptyModelProv :: ModelProv
emptyModelProv = ModelProv Map.empty Map.empty

---------------------
-- NAME PROVENANCE --
---------------------
--
--
deriveNameProvs :: Theory -> ElementProvs -> Model -> NameProv
deriveNameProvs thy prov mdl = Map.fromList $ map (\e@(actual, equal)->(actual, (nameTheorySub (preprocess thy) prov mdl e))) (Map.toList (modelElements mdl))
--
--
nameTheorySub :: Theory -> ElementProvs -> Model -> (Element, [Element]) -> (TheorySub, [Element])
nameTheorySub thy prov mdl (elm, eqelms) = do
  case (getSkolemTree prov mdl elm) of
    Nothing -> (Map.empty, [])
    Just actual@(e, f, r) -> do
      let equal = catMaybes $ map (getSkolemTree prov mdl) (delete elm eqelms)
      let trees = actual:equal
      let irules = map (\r->((fromMaybe 0 (elemIndex r thy)), r)) thy
      (Map.fromList (concatMap (\(i, r)->case nameRuleSub r mdl trees of
        Nothing -> []
        Just rsub -> [(i, rsub)]) irules), r)
--
--
nameRuleSub :: Sequent -> Model -> [(Element, FnSym, [Element])] -> Maybe RuleSub
nameRuleSub rule@(Sequent bd hd) mdl trees = do
  let exists = headExistentials hd 0
  case (catMaybes (map (\(e,f,r)->
    case nameExistSub exists f e of
      Nothing -> Nothing
      Just esub -> Just (esub, nameFreeSub rule r)) trees)) of
    [] -> Nothing
    subs -> do
      let (esubs, fsubs) = unzip subs
      let fnsubs = catMaybes $ (map (\(e,f,r)->nameFuncSub (modelObservations mdl) f e) trees)
      Just $ RuleSub (Map.fromList esubs) (Map.fromList (concat fsubs)) (Map.fromList fnsubs)
--
--
nameExistSub :: [(FnSym, Int, Variable)] -> FnSym -> Element -> Maybe ((Int,Variable), Term)
nameExistSub exists fn elm = do
  let matches = filter (\(f, i, v)->(f==fn)) exists
  case matches of
    [] -> Nothing
    match -> do
      let (f, i, v) = head match
      Just ((i, v), (Elem elm))
--
--
nameFreeSub :: Sequent -> [Element] -> [(Variable, Term)]
nameFreeSub rule elms = do
  let frees = freeVars rule
  let terms = map (\e->(Elem e)) elms
  (zip frees terms)
--
--
nameFuncSub :: [Observation] -> FnSym -> Element -> Maybe ((FnSym, [Term]), Term)
nameFuncSub obvs fn elm = do
  case (filter (\(Obs o)-> 
    case o of
      (Rel _ _) -> False
      (FnRel fnsym _) -> (fnsym==fn)) obvs) of
    [] -> Nothing
    match -> do
      case (head match) of
        (Obs (FnRel fnsym ts)) -> case (length ts) of
          0 -> Nothing
          1 -> Just $ ((fnsym, (tail ts)), (Elem elm))
          _ -> Just $ ((fnsym, (init ts)), (Elem elm))
        _ -> Nothing

----------------------
-- BLAME PROVENANCE --
----------------------
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
getBlamedSequent :: SATTheoryType -> Blame -> Maybe ObservationSequent
getBlamedSequent satthy blame = blameSequent satthy blame
--
--
deriveBlameProv :: Theory -> ProvInfo -> Model -> BlameProv
deriveBlameProv thy prov mdl = do
  let theoryBlames = catMaybes (map (\obv->(possibleAtoms (observationProvs prov) mdl obv)) (modelObservations mdl))
  Map.fromList $ map (\(terms, thyblame, atoms)->(atoms, (blameTheorySub (preprocess thy) (elementProvs prov) mdl terms thyblame))) theoryBlames
--
--
possibleAtoms :: ObservationProvs -> Model -> Observation -> Maybe ([Term], Blame, [Atom])
possibleAtoms prov mdl obv@(Obs (Rel sym ts)) = case (Map.lookup obv prov) of
  Just thyblame -> do
    let eqelms = (map (getEqualElements mdl) ts)
    let possibilities = combination eqelms
    let atms = map (\ts->(Rel sym (map(\t->(Elem t))ts))) possibilities
    Just (ts, thyblame, atms)
  Nothing -> Nothing
possibleAtoms _ _ _ = Nothing
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
--
--
blameTheorySub :: Theory -> ElementProvs -> Model -> [Term] -> Blame -> TheorySub
blameTheorySub thy prov mdl terms (TheoryBlame i sub) = do
  let eqelms = map (getEqualElements mdl) terms
  let elms = map (\es->((head es), (tail es))) eqelms
  let thysubs = map (nameTheorySub thy prov mdl) elms
  let rulesubs = catMaybes $ map (\(thysub,_)->(Map.lookup (i-1) thysub)) thysubs
  let namerulesub = (RuleSub Map.empty sub Map.empty)
  Map.fromList $ [((i-1), (concatRuleSubs (namerulesub:rulesubs)))]

------------------------
-- THEORY REPLACEMENT --
------------------------
replaceExists :: Formula -> Int -> ExistSub -> Formula
replaceExists (And f1 f2) i es           = (And (replaceExists f1 i es) (replaceExists f2 i es))
replaceExists (Or f1 f2) i es            = (Or (replaceExists f1 i es) (replaceExists f2 (i+1) es))
replaceExists (Exists fn v f) i es  = case (Map.lookup (i,v) es) of
  Nothing -> (Exists fn v (replaceExists f i es))
  Just t -> substitute (Map.fromList [(v, t)]) f
replaceExists fml _ _                    = fml
--
--
replaceFrees :: Formula -> FreeSub -> Formula
replaceFrees fml fs = substitute fs fml
--
--
replaceFuncs :: Formula -> FuncSub -> Formula
replaceFuncs (And f1 f2) fns           = (And (replaceFuncs f1 fns) (replaceFuncs f2 fns))
replaceFuncs (Or f1 f2) fns            = (Or (replaceFuncs f1 fns) (replaceFuncs f2 fns))
replaceFuncs (Exists fn v f) fns       = (Exists fn v (replaceFuncs f fns))
replaceFuncs atm@(Atm (Rel relsym terms)) fns = (Atm (Rel "" [substituteFunctions fns (Fn relsym terms)]))
replaceFuncs atm@(Atm (FnRel fnsym terms)) fns = (Atm (FnRel "" [substituteFunctions fns (Fn fnsym terms)]))
replaceFuncs fml _ = fml
--
--
substituteFunctions :: FuncSub -> Term -> Term
substituteFunctions fns term@(Fn name terms) = case (Map.lookup (name, terms) fns) of
  Nothing -> (Fn name (map (substituteFunctions fns) terms))
  Just t -> t
substituteFunctions fns term = term

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
getSkolemHead :: Term -> Maybe (FnSym, [Term])
getSkolemHead skolemtree = do
  case skolemtree of
    (Fn skolemhead skolemrest) -> Just (skolemhead, skolemrest)
    (Cons (Constant skolemhead)) -> Just (skolemhead, [])
    _ -> Nothing
--
--
getSkolemElement :: ElementProvs -> Term -> Maybe Element
getSkolemElement prov skolemterm = (findElementWithProv skolemterm prov)
--
--
getSkolemTree :: ElementProvs -> Model -> Element -> Maybe (Element, FnSym, [Element])
getSkolemTree prov mdl elm = case (getElementProv elm prov) of
  [] -> Nothing
  trees -> case (getSkolemHead (head trees)) of
    Nothing -> Nothing
    Just (skolemhead, skolemrest) -> Just (elm, skolemhead, (concatMap (\t->(maybeToList (getSkolemElement prov t))) skolemrest))
--
-- 
headExistentials :: Formula -> Int -> [(FnSym, Int, Variable)]
headExistentials Tru _                   = []
headExistentials Fls _                   = []
headExistentials (And f1 f2) i           = 
    headExistentials f1 i ++ headExistentials f2 i
headExistentials (Or f1 f2) i            = 
    headExistentials f1 i ++ headExistentials f2 (i+1)
headExistentials (Atm a) _                = []
headExistentials (Exists (Just fn) v f) i = (fn, i, v):(headExistentials f i)
headExistentials (Exists Nothing _ f) i  = headExistentials f i
headExistentials (Lone (Just fn) v f unq) i = (fn, i, v):(headExistentials f i)
headExistentials (Lone Nothing _ f _) i  = headExistentials f i
--
-- an observation (for now) is just an atom consisting of only elements
getObservation :: Formula -> Maybe Observation
getObservation (Atm atm@(Rel rsym terms)) = do
  let elms = concat (map (\t->maybeToList (termToElement t)) terms)
  if (length terms) == (length elms)
    then case toObservation atm of
      Just obv@(Obs (Rel rsym terms)) -> Just obv
      _ -> Nothing
    else Nothing
getObservation _ = Nothing
--
--
concatRuleSubs :: [RuleSub] -> RuleSub
concatRuleSubs [] = (RuleSub Map.empty Map.empty Map.empty)
concatRuleSubs [r] = r
concatRuleSubs ((RuleSub es1 fs1 fns1):rs) = do
  let (RuleSub es2 fs2 fns2) = concatRuleSubs rs
  let es' = Map.fromList $ (Map.toList es1)++(Map.toList es2)
  let fs' = Map.fromList $ (Map.toList fs1)++(Map.toList fs2)
  let fns' = Map.fromList $ (Map.toList fns1)++(Map.toList fns2)
  (RuleSub es' fs' fns')
  