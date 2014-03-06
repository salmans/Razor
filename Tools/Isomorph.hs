module Tools.Isomorph

where

import Data.List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Control.Applicative

import Formula.SyntaxGeo

import Chase.Problem.Model
import Chase.Problem.RelAlg.RelAlg
import qualified RelAlg.DB as DB

import Debug.Trace
import Utils.Trace

isomorphic :: Model -> Model -> Bool
isomorphic mdl1 mdl2 =
    all (\test -> test mdl1 mdl2) [ test_Domain
                                  , test_Preds, test_PredsSize
                                  , test_FullTest]

test_Domain :: Model -> Model -> Bool
test_Domain mdl1 mdl2 = 
    length (modelDomain mdl1) == length (modelDomain mdl2)

test_Preds :: Model -> Model -> Bool
test_Preds mdl1 mdl2 = 
    keys1 == keys2
    where keys1 = (sort.Map.keys) $ modelOrigTables mdl1
          keys2 = (sort.Map.keys) $ modelOrigTables mdl2

-- Assumes that test_Preds returns true
test_PredsSize :: Model -> Model -> Bool
test_PredsSize mdl1 mdl2 = 
    let tablePairs = zipWith (\(_, t1) (_, t2) -> 
                              (DB.toList t1, DB.toList t2)) list1 list2
    in  all (\(t1, t2) -> length t1 == length t2) tablePairs
    where list1 = sortBy sortFunc $ Map.toList $ modelOrigTables mdl1
          list2 = sortBy sortFunc $ Map.toList $ modelOrigTables mdl2
          sortFunc = \(k1, _) (k2, _) -> compare k1 k2

-- Assumes test_PredSize since the result of lookup has to be a Just value.
test_FullTest :: Model -> Model -> Bool
test_FullTest mdl1 mdl2 = 
    any (\p -> mirrorTables (trans p) tbls1 tbls2 (trans' p)) perms
    where dom1   = modelDomain mdl1
          perms  = permutations $ modelDomain mdl2
          tbls1  = modelOrigTables mdl1
          tbls2  = modelOrigTables mdl2
          trans  = \p e -> Maybe.fromJust $ lookup e (zip dom1 p)
          trans' = \p e -> Maybe.fromJust $ lookup e (zip p dom1)

mirrorTables :: (Elem -> Elem) -> Tables -> Tables -> (Elem -> Elem) -> Bool
mirrorTables trans tbls1 tbls2 trans' = 
    let tablePairs = zipWith (\(_, t1) (_, t2) -> (t1, t2)) list1 list2
    in  all (\(t1, t2) -> mirrorTable trans t1 t2 trans') tablePairs
    where list1 = sortBy sortFunc (Map.toList tbls1)
          list2 = sortBy sortFunc (Map.toList tbls2)
          sortFunc = \(k1, _) (k2, _) -> compare k1 k2

mirrorTable :: (Elem -> Elem) -> Table -> Table -> (Elem -> Elem) -> Bool
mirrorTable trans (DB.Set lst1) (DB.Set lst2) trans' = 
    all ((`elem` lst2).(trans <$>)) lst1 && 
        all ((`elem` lst1).(trans' <$>)) lst2