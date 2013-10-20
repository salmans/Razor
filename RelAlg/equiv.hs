{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------------------------
{-| Module      : Equiv -- denotations of equivalence relations
    Copyright   : Fritz Henglein, 2009
    License     : All Rights Reserved

    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
----------------------------------------------------------------------------------------
module RelAlg.Equiv where
import RelAlg.Order
import Char

-- Order T: denotations of total preorders over type T

data Equiv :: * -> *  where

     -- Equiv r : Equivalence relation canonically induced by total preorder r:
     -- x (Equiv r) y <=> x (r) y and y (r) x
     Equiv    :: Order t -> Equiv t

     -- Sum e1 e2 : Canonically induced equivalence relation on tagged values
     -- x (Sum e1 e2) y <=> (x = Left x' and y = Left y' and x' (e1) y')
     --                     (x = Right x' and y = Right y' and x' (e2) y')
     Sum      :: Equiv t1 -> Equiv t2 -> Equiv (Either t1 t2)

     -- Pair e1 e2 : Canonically induced equivalence relation on pairs
     -- (x1, x2) (Pair e1 e2) (y1, y2) <=> x1 (e1) y1 and x2 (e2) y2
     Pair     :: Equiv t1 -> Equiv t2 -> Equiv (t1, t2)

     -- Map f e : Inverse image of e under f
     -- x (Map f e) y <=> f x (e) f y
     Map      :: (t1 -> t2) -> Equiv t2 -> Equiv t1

     -- And e1 e2 : Greatest lower bound (intersection) of e1 and e2
     -- x (And e1 e2) y <=> x (e1) y and x (e2) y
     And      :: Equiv t -> Equiv t -> Equiv t

-- eq e : Extension of e as equality relation (characteristic function)
eq :: Equiv t -> t -> t -> Bool
eq (Equiv r) x y = lte r x y && lte r y x
eq (Sum e1 e2) (Left x) (Left y) = eq e1 x y
eq (Sum e1 e2) (Right x) (Right y) = eq e2 x y
eq (Sum e1 e2) _ _ = False
eq (Pair e1 e2) (x1, x2) (y1, y2) = eq e1 x1 y1 && eq e2 x2 y2
eq (Map f e) x y = eq e (f x) (f y)
eq (And e1 e2) x y = eq e1 x y && eq e2 x y


-- Definitions of standard equivalence relations

eqNat8 :: Equiv Int
eqNat8 = Equiv nat8

eqNat16 :: Equiv Int
eqNat16 = Equiv nat16

eqNat32 :: Equiv Int
eqNat32 = Equiv nat32

eqChar8 :: Equiv Char
eqChar8 = Equiv char8                    -- Equiv (MapO ord (Nat 255))
-- alternative: eqChar8 = Map ord eqNat8 (= Map ord (Equiv (Nat 255)))

list :: Equiv t -> Equiv [t]
list e = Map fromList (Sum (Equiv Unit) (Pair e (list e)))
   where fromList :: [a] -> Either () (a, [a])
         fromList [] = Left ()
         fromList (x : xs) = Right (x, xs)

eqString8 :: Equiv String
eqString8 = Equiv string8

{- 
Equivalences: 
Equiv (Map0 f r) = Map f (Equiv r)
Equiv (SumL r1 r2) = Sum (Equiv r1) (Equiv r2)
Equiv (PairL r1 r2) = Pair (Equiv r1) (Equiv r2)
Equiv (Refine r1 r2) = And (Equiv r1) (Equiv r2)
Equiv (Inv r) = Equiv r
-}

-- normalize : normalize an equivalence denotation by pushing out Equiv constructors
normalize :: Equiv t -> Equiv t
normalize (Equiv (MapO f r)) = Map f (normalize (Equiv r))
normalize (Equiv (SumL r1 r2)) = Sum (normalize (Equiv r1)) (normalize (Equiv r2))
normalize (Equiv (PairL r1 r2)) = Pair (normalize (Equiv r1)) (normalize (Equiv r2))
normalize (Equiv (Refine r1 r2)) = And (normalize (Equiv r1)) (normalize (Equiv r2))
normalize (Equiv (Inv r)) = normalize (Equiv r)
normalize (Sum e1 e2) = Sum (normalize e1) (normalize e2)
normalize (Pair e1 e2) = Pair (normalize e1) (normalize e2)
normalize (Map f e) = Map f (normalize e)
normalize (And e1 e2) = And (normalize e1) (normalize e2)
normalize e = e