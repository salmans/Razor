{-| Time-stamp: <2013-05-14 11:52:48 Salman Saghafi>
  This module defines a Model structure that will be used inside a Problem structure. This module can be redefined based on the underlying implementation for models.
-}
module Chase.Problem.IModel where

-- General Modules
import Data.List
import qualified Data.Map as Map
import Data.Maybe

-- Logic Modules
import Formula.SyntaxGeo
import Utils.GeoUtilities

-- Chase Modules:
import Chase.Problem.Observation
import qualified CC.CC as CC

{- Errors raised by this modeule -}
err_ChaseProblemModel_IsTruthDen =
    "Chase.Problem.Model.isTruth: No truth value for Den"
err_ChaseProblemModel_NoEqToDen =
    "Chase.Problem.Model.obsToEquation: Cannot convert Den to Equation"
err_ChaseProblemModel_DelDenotes =
    "Chase.Problem.Model.denotes: Not applicable on Den"
err_ChaseProblemModel_EqlDenotes =
    "Chase.Problem.Model.denotes: Not applicable on Eql"

{-| A modle is a rewrite system together with a set of special constants in the rewrite system.
-}
data Model = Model {
      modelTRS :: CC.TRS,
      modelDomain :: [Term]
}

instance Show Model where
    show (Model trs domain) =
        "\n" ++
        "----------------- Model -----------------\n" ++
        "Domain: " ++ domString ++ "\n\n" ++
        "Functions: " ++ funcString ++ "\n\n" ++
        "Relations:" ++ relString ++ "\n" ++
        "-----------------------------------------\n"
        where funcString = printNone (mergeLines (funcGroupToString (groupFunction (nub (sortFuncAsNumber func)))))
              relString = printNone (mergeLines (relGroupToString (groupRelation (nub (sortRelAsNumber rel)))))
              domString = printNone (intercalate "," (map show (sortDomAsNumber (removeTrue dom))))
              (func, rel) = showFactsInTerms trs trs
              dom = nub (showDomain trs)

{--- Common helper functions ---}

{- Returns (none) if the string is empty; otherwise, returns the same string -}
printNone :: String -> String
printNone str =
    if str == ""
    then "(none)"
    else str

{- Converts Elm into numbers, by also removing a prefix "const" -}
numberElmToInt :: Term -> Int
{- Removes "const" if possible -}
numberElmToInt (Elm ('c':'o':'n':'s':'t' : num)) = (read num::Int)
{- Otherwise, just read the number -}
numberElmToInt (Elm num) = (read num::Int)

{- Converts (Fn name parameters) into (name, parameters as ints) -}
funcStringToInt :: Term -> (String, [Int])
funcStringToInt (Fn name params) = (name, map numberElmToInt params)
funcStringToInt _ = ("",[])

{- Converts list of ints into a string by keeping the left string the same.
   String represents Fn names; ints, parameters -}
intListToString :: (String, [Int]) -> (String, String)
intListToString (name, intParams) =
    if (length intParams) == 1
    then (name, intercalate "," (map show intParams))
    else (name, "(" ++ (intercalate "," (map show intParams)) ++ ")")

{- Merges strings together by separating them with newlines.
   String represents one group of either functions or relations -}
mergeLines :: [String] -> String
mergeLines sl = concat (map makeNewLine sl)
    where makeNewLine s = "\n - " ++ s


{--- Functions for processing domain output ---}

{- Sorts the domain list by comparing them as numbers
   (e.g., [3,20,1] will be sorted as [1,3,20], not [1,20,3] as like strings).
   The return value is the sorted list of integers -}
sortDomAsNumber :: [Term] -> [Int]
sortDomAsNumber tl =
    let presort = map numberElmToInt tl in
    sort presort

{- Removes Elm "True" (from the domain list) -}
removeTrue :: [Term] -> [Term]
removeTrue tl = filter notTruth tl
    where notTruth t = (t /= truth)


{--- Functions for processing function output ---}

{- Sorts the (Fn, value) pair list by comparing number-strings as numbers (see sortDomAsNumber),
   by name first, then parameter list (value doesn't matter since a function has at most one value).
   The return value is the list of (Fn name, parameters, value) as string triples. -}
sortFuncAsNumber :: [(Term, Term)] -> [(String, String, String)]
sortFuncAsNumber tl =
    let presort = map funcStringToIntLeft tl in
    let postsort = sort presort in
    map intListToStringAll postsort
    where funcStringToIntLeft (t1, t2) = ((funcStringToInt t1), t2) -- (Term1, Term2) -> ((String, [Int]), Term2)
          intListToStringAll (i1, t2) =
              let (fname, fpl) = (intListToString i1) in
              let fval = show (numberElmToInt t2) in
                  (fname, fpl, fval) -- All strings

{- Convert a group of same-name functions into string
   (e.g., ("f", [("(1,2)","3") , ("(1,3)","0")])
   to "f: (1,2)->3, (1,3)->0" ) -}
funcGroupToString :: [(String, [(String, String)])] -> [String]
funcGroupToString ((n,pvl):l) = (n ++ ": " ++ (intercalate ", " (map mergePair pvl))) : (funcGroupToString l)
    where mergePair (params, value) = params ++ "->" ++ value
funcGroupToString _ = []

{- Groups functions of the same name together
   (e.g., [(f,(1,2),3), (f,(1,3),0)])
   to (f, [((1,2),3) , ((1,3),0)]) -}
groupFunction :: [(String, String, String)] -> [(String, [(String, String)])]
groupFunction l =
    let extractName (name,_,_) = (name,[]) in
    let nameList = nub (map extractName l) in
    groupFunctionNames l nameList

{- Groups function names (helper function of groupFunction).
   Input: (Fn name, parameters, value) as string triples; (Fn name, [(param-value pair)]) as strings, an accumulator
   Output: the same accumulator -}
groupFunctionNames :: [(String, String, String)] -> [(String, [(String, String)])] -> [(String, [(String, String)])]
groupFunctionNames ((name,params,value):s3l) accum = groupFunctionNames s3l (map addParamIfNameMatched accum)
    where addParamIfNameMatched (n,pvl) =
            if name == n
                then (n, ((params,value):pvl))
                else (n, pvl)
{- In the end, reverses parameter lists back -}
groupFunctionNames _ accum = map sortParamList accum
    where sortParamList (name, paramValueList) = (name, (reverse paramValueList))


{--- Functions for processing function output ---}

{- Sorts the Fn (relation) list by comparing number-strings as numbers (see sortDomAsNumber),
   by name first, then parameter list.
   The return value is the list of (Fn name, parameters) as string pairs. -}
sortRelAsNumber :: [Term] -> [(String, String)]
sortRelAsNumber tl =
    let presort = map funcStringToInt tl in
    let postsort = sort presort in
    map intListToString postsort

{- Convert a group of same-name relations into string
   (e.g., ("R", ["(1,2)", "(1,3)"])
   to "R: (1,2),(1,3)" ) -}
relGroupToString :: [(String, [String])] -> [String]
relGroupToString ((n,pl):l) = (n ++ ": " ++ (intercalate "," pl)) : (relGroupToString l)
relGroupToString _ = []

{- Groups relations of the same name together
   (e.g., [(R,(1,2)), (R,(1,3))])
   to (R, [(1,2), (1,3)]) -}
groupRelation :: [(String, String)] -> [(String, [String])]
groupRelation l =
    let extractName (name,_) = (name,[]) in
    let nameList = nub (map extractName l) in
    groupNames l nameList

{- Groups relation names (helper function of groupRelation).
   Input: (Fn name, parameters) as string pairs; (Fn name, [params]) as strings, an accumulator
   Output: the same accumulator -}
groupNames :: [(String, String)] -> [(String, [String])] -> [(String, [String])]
groupNames ((name,params):s2l) accum = groupNames s2l (map addParamIfNameMatched accum)
    where addParamIfNameMatched (n,pl) =
            if name == n
                then (n, (params:pl))
                else (n, pl)
{- In the end, reverses parameter lists back -}
groupNames _ accum = map sortParamList accum
    where sortParamList (name, paramList) = (name, (reverse paramList))


{- Lists the values of functions and relations of a model in a pair of
   term-term pairs and terms.
-}
showFactsInTerms :: CC.TRS -> CC.TRS -> ([(Term, Term)], [Term]) -- funcs and rels
showFactsInTerms _ [] = ([], [])
showFactsInTerms trs rs =
    foldr augment ([],[]) rs
    where augment (CC.RW t1@(Fn _ _) _) (funcs, rels) =
              let t1' = CC.normalForm trs t1
              in if t1' == truth
                 then (funcs, t1:rels)
                 else ((t1,t1'):funcs, rels)
          augment _ (funcs, rels) = (funcs, rels)


{- Compute the set of elements in the domain that are displayed to the user. -}
showDomain :: CC.TRS -> [Term]
showDomain trs =
    foldr augment [] trs
    where augment (CC.RW t1 _) = (CC.normalForm trs t1:)

{-| truth is an special element represented by "True" in models. -}
truth :: Term
truth = Elm "True"

{-| A shorthand for an empty model. -}
empty :: Model
empty = Model [] []

{-| Adds a list of new observations to the given model. It also returns a set of rewrite rules that are introduced to the underlying rewrite system as the new observations are being added.
-}
add :: Model -> [Obs] -> (Model, [CC.RWRule])
add model@(Model trs consts) obs = 
    (Model newTRS newConsts, newRules)
    where ((newTRS, newConsts), newRules) = CC.buildTRS eqs (trs, consts)
          eqs = map obsToEquation obs

{- Convert an obs to a CC.Equation -}
obsToEquation :: Obs -> CC.Equation
obsToEquation (Den t) = error err_ChaseProblemModel_NoEqToDen
obsToEquation (Eql t1 t2) = CC.Eql t1 t2
obsToEquation (Fct a) = CC.Eql (fromJust (toTerm a)) truth

{-| Returns true if a term is true in the model; That is, if the Obs is "Fct a", it verifies whether t is true in the model. If the Obs is "Eql t1 t2", it verifies whether t1 and t2 are equal in the model. -} 
isTrue :: Model -> Obs -> Bool
isTrue (Model trs _) obs@(Den t) = 
    case t == truth of -- Since we treat truth as a denotation, truth is an 
                       -- exceptional case.
      True -> True
      False -> error $ err_ChaseProblemModel_IsTruthDen
isTrue (Model trs _) obs@(Fct a) = CC.normalForm trs (obsToTerm obs) == truth
isTrue (Model trs _) (Eql t1 t2) = nf t1 == nf t2
    where nf = CC.normalForm trs

{-| Returns an element in the domain of the model to which a given Fct observation denotes. If the observation is an equation, the function reduces the two sides of the equation to what they denote.
-}
denotes :: Model -> Obs -> Obs
denotes (Model trs _)  obs@(Den t) = 
    if t == truth 
    then obs -- Because truth is an exceptional denotation that can behave 
             -- like a fact, this function is only applicable on truth but 
             -- not other denotations.
    else error err_ChaseProblemModel_DelDenotes
denotes (Model trs _) (Eql t1 t2) = Eql (nf t1) (nf t2)
    where nf = CC.normalForm trs
denotes (Model trs _) obs = 
    if nf == truth
    then termToObs False truth --Always convert truth to Den
    else termToObs True nf
         -- HINT: look at the documentation for termToObs to understand
         -- the first boolean argument.
    where nf = CC.normalForm trs $ obsToTerm obs
