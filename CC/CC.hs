{- Time-stamp: <2013-05-14 11:52:01 Salman Saghafi>

   This module, contains our primary model-finding algorithm
   that will use other modules.
-}
module CC.CC(RWRule(..), Equation(..), buildTRS, normalForm) where

{- Specify the CC module to use -}
import CC.Naive