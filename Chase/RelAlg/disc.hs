{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------------------------
{-| Module      : Discriminators
    Copyright   : Fritz Henglein, 2009
    License     : All Rights Reserved
    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
----------------------------------------------------------------------------------------
module Chase.RelAlg.Disc where

import Chase.RelAlg.Order
import Chase.RelAlg.Equiv
import Chase.RelAlg.NatDisc

type Disc k = forall v. [(k, v)] -> [[v]]

-- Basic multiset discriminator with table of size 2^16 = 65536.  
-- Keys must have type Int and be in range [0 .. 2^16-1]
discNat16 :: [(Int, v)] -> [[v]]
discNat16 = discNat 65536

discNat32 :: [(Int, v)] -> [[v]]
discNat32 = disc eqNat32

disc :: Equiv k -> Disc k
disc e = disc' (normalize e)


disc' :: Equiv k -> Disc k

disc' e []               = []
disc' e [(k, v)]         = [[v]]

disc' (Equiv (Nat n)) xs = if n > 65536 
                           then discNat32 xs 
                           else discNat16 xs

disc' (Equiv Unit) xs    = [[ v | (_, v) <- xs ]]

disc' (Sum e1 e2) xs    =  disc' e1 [ (k, v) | (Left k, v) <- xs ] 
                        ++ disc' e2 [ (k', v') | (Right k', v') <- xs ]

disc' (Pair e1 e2) xs    = discs e2 (disc' e1 [ (k1, (k2, v)) | ((k1, k2), v) <- xs ])
     where discs e xss = [ vs | xs <- xss, vs <- disc' e xs ]

disc' (Map f e) xs       = disc' e [ (f k, v) | (k, v) <- xs ] 

disc' (And e1 e2) xs     = disc' (Pair e1 e2) [((k, k), v) | (k, v) <- xs]

-- Partitioning from discrimination

part :: Equiv t -> [t] -> [[t]]
part e xs = disc e [ (x, x) | x <- xs ]

-- Representatives from partitioning

reps :: Equiv t -> [t] -> [t]
reps e xs = [ head ys | ys <- part e xs ]
