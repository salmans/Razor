{-|
  Razor
  Module      : API.Surface
  Description : The module provides a surface API for user level commands interacting with the core API
  Maintainer  : Salman Saghafi, Ryan Danas
-}
module API.Surface where
import API.Core
import Common.Model
import Common.Provenance
import Syntax.GeometricUtils
import SAT.Impl
import Tools.Config
import Data.Either
import Data.Maybe
import System.Environment

data UError = UErr String
type UState = (Theory, ProvInfo, SATIteratorType, Model)
type UTheorySubs = [Maybe Sequent]
data UOrigin = UOriginLeaf Term (Either UError UTheorySubs) | UOriginNode Term (Either UError UTheorySubs) [UOrigin]

-- TODO
-- 1. UTheorySubs is not representative of what we actually want
--  both origin and blaming infomation is just shown to the user through substitutions...
--  UOrigin = Element UTheorySubs; "origin of element e0... for rule #m, do these replacements, and for rule #n do these replacements... "
--  UBlame = Fact UTheorySubs; "justification of Student(e8)... for ... you get the idea"
--  TheorySubs = [(Int, USequentSub)]
--  USequentSub = [FreeVarSub] [ExistSub] [ConstSub] [FuncSub]
--    FreeVarSub = Variable Element; "every time you see x, replace it with e0"
--    ExistSub = Int Variable Element; "in the nTH disjunct of the head, replace every x with e0, remove any instance of 'exists e0.'"
--    ConstSub = Constant Element; "every time you see 'ALAS, replace it with e0"
--    FuncSub = Function Element; "f(g(e0)) is actually e4"
-- 2. UOrigin/UBlame should eventually replace ProvInfo???
--  not sure about this; just have API.Core deal with creating the ideal data structures (TODO #1) / being able to do replacements?

getConfig :: IO Config
getConfig = do 
	args <- getArgs
	parseConfig args

getStartState :: Config -> IO (Either UError UState)
getStartState config = do
  case (configInput config) of
    Nothing -> return $ Left (UErr "No input file specified")
    Just infile -> do
      input <- readFile infile
      theory <- parseTheory config input
      case theory of
        Just thy -> do
          let (base, prov, prop) = generateGS config thy
          let stream = modelStream prop
          case (nextModel stream) of
            (Nothing, stream') -> return $ Left (UErr "no minimal models available")
            (Just model', stream') -> return $ Right (thy, prov, stream', model')
        Nothing -> return $ Left (UErr "Unable to parse input theory!")

getNextModel :: UState -> Either UError UState
getNextModel state@(thy, prov, stream, model) = case (nextModel stream) of
  (Nothing, stream') -> Left (UErr "no more minimal models")
  (Just model', stream') -> Right (thy, prov, stream', model')

getOrigin :: UState -> (Bool, Bool) -> Term -> UOrigin
getOrigin state@(theory, prov, stream, model) mods@(isall, isrec) term = do
  case name of
    Left err -> UOriginLeaf term (Left err)
    Right (namedtheory, nextterms) -> do
      case isrec of
        False -> UOriginLeaf term (Right namedtheory)
        True -> UOriginNode term (Right namedtheory) (map (getOrigin state mods) nextterms)
  where 
    name = case (getEqualElements model term) of
      [] -> Left (UErr ("element "++(show term)++" not in the current model"))
      eqelms -> do
        case (getSkolemTrees prov model term) of
          [] -> Left (UErr ("no provenance information for element "++(show term)++"\n"))
          skolemtrees -> do
            case isall of
              True -> do
                let (actualelm,_,_) = head skolemtrees
                let allelms = map (\(e, h, r)->e) skolemtrees
                let skolemnext = concat (map (\(e, h, r)->r) skolemtrees)
                let names = (map (\(e, h, r)->(actualelm, h, r)) skolemtrees) 
                let namedtheory = nameTheory theory names
                let nextterms = (map (\e->(Elem e)) skolemnext)
                return (namedtheory, nextterms)
              False -> do
                let (elm, skolemhead, skolemnext) = (head skolemtrees)
                let namedtheory = (nameTheory theory [(elm, skolemhead, skolemnext)])
                let nextterms = (map (\e->(Elem e)) skolemnext)
                return (namedtheory, nextterms)

getJustification :: UState -> Formula -> Either UError UTheorySubs
getJustification state@(theory, prov, stream, model) atom = case (getFact model atom) of
  Nothing -> Left (UErr "fact not in form FactName(e^0, e^1, ...) or is not in the current model")
  Just fact@(factname, factelms) -> case (getBlame prov model fact) of
    [] -> Left (UErr ("no provenance information for fact "++(show atom)))
    blames -> do
      let names = (concatMap (\t -> do
                                      let skolemtrees = (getSkolemTrees prov model t) 
                                      let (actualelm,_,_) = head skolemtrees
                                      map (\(e, h, r)->(actualelm, h, r)) skolemtrees) factelms)
      let blamedtheory = (blameTheory theory names blames)
      Right blamedtheory
