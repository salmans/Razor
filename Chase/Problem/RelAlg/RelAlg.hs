{-|
  A wrapper around IRelAlg.
-}
module Chase.Problem.RelAlg.RelAlg 
    (Record, Table, TableRef (..), Tables,
     RelExp (..), Labels, Equation (..),
     mergeSets, mergeAllSets, mergeSetsWith, mergeAllSetsWith,
     mergeSetsWithKey, mergeAllSetsWithKey,
     unionSets, nubSet,
     headRelExp, bodyRelExp, emptyTables,
     evaluateRelExp, diff) where
import Chase.Problem.RelAlg.IRelAlg