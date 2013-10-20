{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------------------------
{-| Module      : Order -- denotations of total preorders
    Copyright   : Fritz Henglein, 2009
    License     : All Rights Reserved
    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
----------------------------------------------------------------------------------------
module RelAlg.Order where
import Data.Bits
import Char

-- Order T: denotations of total preorders over type T

data Order :: * -> *  where

     -- Nat n: standard ascending order on [0..n], 
     -- x (Nat n) y <=> x <= y
     Nat      ::  Int -> Order Int  

     -- Unit : trivial order on ()
     -- () (Unit) ()
     Unit     ::  Order ()        

     -- SumL r1 r2 : order on tagged values, left elements ordered according to r1,
     --             right elements according to r2; left elements first
     -- x (SumL r1 r2) y <=> (x = Left x' and (y = Left y' => x' r1 y')
     SumL     ::  Order t1 -> Order t2 -> Order (Either t1 t2)

     -- PairL r1 r2 : lexicographic order on pairs, ordered by first components according to r1,
     --            with equivalent first components ordered by second components according to r2
     -- (x1, x2) (PairL r1 r2) (y1, y2) <=> x1 (r1) y1 and (y1 (r1) x1 => x2 (r2) y2)
     PairL    ::  Order t1 -> Order t2 -> Order (t1, t2)

     -- MapO f r : order on t1 induced by mapping t1-elements to 
     --           t2-elements ordered according to r
     -- x (MapO f r) y <=> (f x) (r) (f y)
     MapO     ::  (t1 -> t2) -> Order t2 -> Order t1 

     -- Refine r r': Refine order r by ordering r-equivalent elements according to r'
     -- x (Refine r1 r2) y <=> x (r1) y and (y (r1) x => x (r2) y)
     Refine   ::  Order t -> Order t -> Order t

     -- Inv r: Inverse order of r
     -- x (Inv r) y <=> y (r) x
     Inv      ::  Order t -> Order t 

-- lte r : extension of r as relation (characteristic function)
lte :: Order t -> t -> t -> Bool
lte (Nat n) x y = x <= y  -- size check omitted 
lte Unit _ _ = True
lte (SumL r1 r2) (Left x) (Left y) = lte r1 x y
lte (SumL r1 r2) (Left _) (Right _) = True
lte (SumL r1 r2) (Right _) (Left _) = False
lte (SumL r1 r2) (Right x) (Right y) = lte r2 x y
lte (PairL r1 r2) (x1, x2) (y1, y2) = lte r1 x1 y1 && if lte r1 y1 x1 then lte r2 x2 y2 else True
lte (MapO f r) x y = lte r (f x) (f y)
lte (Refine r1 r2) x y = lte r1 x y && if lte r1 y x then lte r2 x y else True
lte (Inv r) x y = lte r y x

-- Definitions of standard orderings

-- nat8 : standard ascending order on Int, restricted to 8-bit nonnegative integers
nat8 :: Order Int
nat8 = Nat 255

-- nat16 : standard ascending order on Int, restricted to 16-bit nonnegative integers
nat16 :: Order Int
nat16 = Nat 65535

-- nat32 : standard ascending order on Int, restricted to 32-bit nonnegative integers
nat32 :: Order Int 
nat32 = MapO splitW (PairL nat16 nat16)
  where splitW :: Int -> (Int, Int)
        splitW x = (shiftR x 16 .&. 65535, x .&. 65535)

-- char8 : standard alphabetic order on Char, restricted to 8-bit characters 
char8 :: Order Char        
char8 = MapO ord nat8

-- char16 : standard alphabetic order on Char, restricted to 16-bit characters 
char16 :: Order Char        
char16 = MapO ord nat16

-- fromList: unfold-part of isomorphism between [t] and Either () (t, [t])
fromList ::  [t]       ->  Either () (t, [t])
fromList     []        =   Left ()
fromList     (x : xs)  =   Right (x, xs)

-- Lexicographic ordering on lists, with elements ordered according to r
listL :: Order t -> Order [t]
listL r = MapO fromList (SumL Unit (PairL r (listL r)))

-- string : standard alphabetic order on String, restricted to 8-bit characters
string8 :: Order String
string8 = listL char8

-- Ordering of tagged values, but with Right elements first
sumR :: Order t1 -> Order t2 -> Order (Either t1 t2)
sumR r1 r2 = MapO flip (SumL r2 r1)
   where flip :: Either t1 t2 -> Either t2 t1
         flip (Left x) = Right x
         flip (Right y) = Left y

-- Lexicographic ordering on pairs, but with second component dominant
pairR :: Order t1 -> Order t2 -> Order (t1, t2)
pairR r1 r2 = MapO swap (PairL r2 r1)
    where swap :: (t1, t2) -> (t2, t1)
          swap (x, y) = (y, x)
