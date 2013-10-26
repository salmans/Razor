{-|
  A wrapper around IRelAlg.
-}
module Chase.Problem.RelAlg.RelAlg 
    (Record, Table, TableRef (..), Tables,
     RelExp (..), Labels, Equation (..),
     mergeSets, mergeAllSets, emptyTables,
     headRelExp, bodyRelExp,
     evaluateRelExp, diff) where
import Chase.Problem.RelAlg.IRelAlg