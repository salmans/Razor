{-|
  A wrapper around IRelAlg.
-}
module Chase.Problem.RelAlg.RelAlg 
    ( Record, Table, TableRef (..), Tables, RelExp (..), Labels
    , mergeSets, mergeAllSets, mergeSetsWith, mergeAllSetsWith
    , mergeSetsWithKey, mergeAllSetsWithKey
    , unionSets, nubSet
    , headRelExp, bodyRelExp, emptyTables, delta
    , evaluateRelExp, diff) where
import Chase.Problem.RelAlg.IRelAlg