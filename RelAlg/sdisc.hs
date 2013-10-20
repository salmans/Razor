{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------------------------
{-| Module      : Discriminators
    Copyright   : Fritz Henglein, 2007
    License     : All Rights Reserved

    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
----------------------------------------------------------------------------------------
module SDisc where

import Order 
import NatDisc

type Disc k = forall v. [(k, v)] -> [[v]]

-- Generic definition of stable ordered discriminator 
sdisc :: Order k -> Disc k

sdisc r []               = []
sdisc r [(k, v)]         = [[v]]

sdisc (Nat n) xs         = sdiscNat2 n xs

sdisc Unit xs            = [[ v | (_, v) <- xs ]]

sdisc (SumL r1 r2) xs    =  sdisc r1 [ (k, v) | (Left k, v) <- xs ] 
                         ++ sdisc r2 [ (k', v') | (Right k', v') <- xs ]

sdisc (PairL r1 r2) xs   = [ vs | ys <- sdisc r1 [ (k1, (k2, v)) | ((k1, k2), v) <- xs ],
                                  vs <- sdisc r2 ys ]

sdisc (MapO f r) xs      = sdisc r [ (f k, v) | (k, v) <- xs ] 

sdisc (Refine r1 r2) xs  = sdisc (PairL r1 r2) [ ((k, k), v) | (k, v) <- xs ]

sdisc (Inv r) xs         = reverse (sdisc r xs)

-- Sorted partitioning from sorted discrimination

spart :: Order t -> [t] -> [[t]]
spart r xs = sdisc r [ (x, x) | x <- xs ]

-- Discriminative sorting from sorted partitioning

dsort :: Order t -> [t] -> [t]
dsort r xs = [ y | ys <- spart r xs, y <- ys ]

-- Unique sorting from ordered partitioning

usort :: Order t -> [t] -> [t]
usort r xs = [ head ys | ys <- spart r xs ]

-- More order denotations

bag :: Order t -> Order [t]
bag r = MapO (dsort r) (listL r)

set :: Order t -> Order [t]
set r = MapO (usort r) (listL r)

