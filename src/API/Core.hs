{-|
  Razor
  Module      : API.Core
  Description : The module provides the core API layer for interacting with the functional core of razor
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module API.Core where
import Chase.Impl
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
import Tools.FolToGeo (parseFolToSequents)
import Tools.Utils (isRealLine)

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
            "XML FILE")
        "UserState represented as XML (returned by CLI)"
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
	let sequents = mapM (parseFolToSequents False) (filter isRealLine inputLines)
	return $ concat <$> sequents
-- In: configuration, theory
-- Out: G*, which consists of ground facts, provenance info, and a propositional theory
generateGS :: Config -> Theory -> (ChaseHerbrandBaseType, ProvInfo, SATTheoryType)
generateGS config theory = chase config theory
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
instance Show RuleSub where
  show (RuleSub n f fn) = (show n)++"\n"++(show f)++"\n"++(show fn)++"\n"
type FreeSub = Sub
type ExistSub = Map.Map (Int,Variable) Term
type FuncSub = Map.Map (FnSym, [Term]) Term
--
--
deriveModelProv :: Theory -> ProvInfo -> Model -> ModelProv
deriveModelProv thy prov mdl = ModelProv (deriveNameProvs thy (elementProvs prov) mdl) (deriveBlameProv (observationProvs prov) mdl)

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
deriveBlameProv :: ObservationProvs -> Model -> BlameProv
deriveBlameProv prov mdl = Map.empty --Map.fromList $ concatMap (\obv->(possibleObservations prov mdl obv)) (modelObservations mdl)
--
--
possibleObservations :: ObservationProvs -> Model -> Observation -> [(Atom, Int)]
possibleObservations prov mdl obv@(Obs (Rel sym ts)) = case (Map.lookup obv prov) of
  Just (TheoryBlame i sub) -> do
    let 
    possibility <- combination (map (getEqualElements mdl) ts)
    return ((Rel sym (map (\t->(Elem t)) possibility)), i)
  Nothing -> []
possibleObservations _ _ _ = []
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













-- In: prov of a theory, and a term representing an element
-- Out: the skolem tree if it exists 
getSkolemTrees :: ElementProvs -> Model -> Term -> [(Element, FnSym, [Element])]
getSkolemTrees prov mdl term = do
  -- turn the parsed term into an element
  case (termToElement term) of
    Nothing -> []
    Just elm -> do
      -- get the provenance skolem tree for this element
      case (getElementProv elm prov) of
        [] -> []
        trees -> case (getSkolemHead (head trees)) of
          Nothing -> []
          Just (skolemhead, skolemrest) -> do
            let actual = (elm, skolemhead, (concatMap (\t->(maybeToList (getSkolemElement prov t))) skolemrest))
            let equal = (concat (map (getSkolemTrees prov mdl) (map (\t->(Elem t)) (delete elm (getEqualElements mdl (Elem elm))))))
            actual:equal
-- In: element, skolem function name, theory
-- Out: a theory with the associated exists variable in every related sequent replaced by the element
nameTheory :: Theory -> [(Element, FnSym, [Element])] -> [Maybe Sequent]
nameTheory thy names = map (nameSequent names) (zip thy (preprocess thy))
-- In: element, skolem function name, sequent in theory
-- Out: a sequent with the associated exists variable in the head replaced by the element
nameSequent :: [(Element, FnSym, [Element])] -> (Sequent, Sequent) -> Maybe Sequent
nameSequent names ((Sequent obdy ohd), (Sequent bdy hd)) = do
  let headnames = map (\(e, h, r)->(h, e)) names
  let rests = map (\(e, h, r)->r) names
  let constsubs = Map.fromList (map (\(e, h, r)->((Constant (T.unpack (head (T.splitOn (T.pack "^") (T.pack h))))), (Elem e))) names)
  -- name head existentially quantified variables / lone constants
  case (nameHead headnames (ohd,hd)) of
    Nothing -> Nothing
    -- name body IFF head exists or lone constants were named
    Just newhd -> do
      let frees = (freeVars (Sequent obdy newhd))
      let elms = map (\e->(Elem e)) (concat rests)
      let freesubs = Map.fromList (zip frees elms)
      Just (Sequent (substituteConstants constsubs (substitute freesubs obdy)) (substituteConstants constsubs (substitute freesubs newhd)))
-- In: element, skolem function name, head formula in sequent
-- Out: a formula with the associated exists variable replaced by the element
nameHead :: [(FnSym, Element)] -> (Formula, Formula) -> Maybe Formula
-- Junctions aka Keep looking
nameHead names ((And of1 of2),(And f1 f2)) = case (nameHead names (of1,f1)) of
  Nothing -> case (nameHead names (of2,f2)) of
    Nothing -> Nothing
    Just nf2 -> Just (And of1 nf2)
  Just nf1 -> case (nameHead names (of2,f2)) of
    Nothing -> Just (And nf1 of2)
    Just nf2 -> Just (And nf1 nf2)
nameHead names ((Or of1 of2),(Or f1 f2)) = case (nameHead names (of1,f1)) of
  Nothing -> case (nameHead names (of2,f2)) of
    Nothing -> Nothing
    Just nf2 -> Just (Or of1 nf2)
  Just nf1 -> case (nameHead names (of2,f2)) of
    Nothing -> Just (Or nf1 of2)
    Just nf2 -> Just (Or nf1 nf2)
-- Exists
nameHead names ((Exists ofn ov off),(Exists (Just fn) v ff)) = do 
  let skolemMap = Map.fromList names
  case (Map.lookup fn skolemMap) of
    Just elm -> case (nameHead names (off,ff)) of
      Nothing -> Just (substitute (Map.fromList [(ov, (Elem elm))]) off)
      Just nff -> Just (substitute (Map.fromList [(ov, (Elem elm))]) nff)
    Nothing -> case (nameHead names (off,ff)) of
      Nothing -> Nothing
      Just nfml -> Just (Exists ofn ov nfml)
-- Lone
nameHead names (ofml,fml@(Lone fn _ f _)) = do
  let skolemMap = Map.fromList names
  case (Map.lookup (fromMaybe "" fn) skolemMap) of
    Nothing -> (nameHead names (ofml, f))
    Just elm -> case (nameHead names (ofml, f)) of
      Nothing -> Just ofml
      Just nfml -> Just nfml
-- Everything else aka Nothing to replace
nameHead _ (ofml,_) = Nothing 



--
--
getFact :: Model -> Formula -> Maybe (FnSym, [Term])
getFact mdl fml = case fml of
  (Atm (Rel rsym terms)) -> do
    case (toObservation (Rel rsym terms)) of
      Nothing -> Nothing
      Just obv -> if (elem obv (modelObservations mdl))
        then do
          let elms = concat (map (\t->maybeToList (termToElement t)) terms)
          if ((length terms) == (length elms))
            then Just (rsym, terms)
            else Nothing
        else Nothing
  _ -> Nothing
--
--
getBlame :: ProvInfo -> Model -> (FnSym, [Term]) -> [Blame]
getBlame prov mdl (factname, factelms) = do
  let choices = []--(combination (map (getEqualElements mdl) factelms))
  concat (map (\choice->(getObvProvs prov (factname, choice))) choices)

--
--
getObvProvs :: ProvInfo -> (FnSym, [Term]) -> [Blame]
getObvProvs prov (factname, factelms) = do
  let obv = fromMaybe (Obs (Rel "" [])) (toObservation (Rel factname factelms))
  maybeToList ((findObservationWithProv obv (observationProvs prov)))



--
--
blameTheory :: Theory -> [(Element, FnSym , [Element])] -> [Blame] -> [Maybe Sequent]
blameTheory thy names blames = do
  let namedthy = nameTheory thy names
  let blamereplaces = Map.fromList (map (\(TheoryBlame i s)->(i, s)) blames)
  (sequent, namedsequent) <- zip thy namedthy
  newsequent <- case (elemIndex sequent thy) of
    Nothing -> return Nothing
    Just i -> case (Map.lookup (i+1) blamereplaces) of
      Nothing -> return Nothing
      Just blamesub -> do
        return (Just (substitute blamesub (fromMaybe sequent namedsequent)))
  return newsequent
  