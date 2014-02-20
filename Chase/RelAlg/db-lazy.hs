{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------------------------
{-| Module      : Relational algebra, with discrimination-based joins and lazy products
    Copyright   : Fritz Henglein, 2009
    License     : All Rights Reserved

    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
----------------------------------------------------------------------------------------
module Chase.RelAlg.DB3 where

import Chase.RelAlg.Equiv
import Chase.RelAlg.Disc

-- Sets, projections and predicates

-- Set a: set of records of type a, with lazy union and cross-product
data Set :: * -> *  where

     -- Set l : Injection of list l into Set
     Set     :: [a] -> Set a
     
     -- s1 `U` s2 : Union of sets s1 and s2
     U        :: Set a -> Set a -> Set a

     -- s1 `X` s2 : Cross product of sets s1 and s2
     X        :: Set a -> Set b -> Set (a, b)

empty :: Set a
empty = Set []

-- Flattening to lists
toList :: Set a -> [a]
toList s = append s [] 
  where append :: Set a -> [a] -> [a]
        append (Set xs) ys = xs ++ ys
        append (s1 `U` s2) ys = append s1 (append s2 ys)
        append (s1 `X` s2) ys = [ (v1, v2) | v1 <- toList s1, v2 <- toList s2 ] ++ ys

-- size s: storage size of a an element of type Set a
--         where each element of a is assumed to have size 1
size :: Set a -> Int
size (Set xs) = 1 + length xs
size (s1 `U` s2) = 1 + size s1 + size s2
size (s1 `X` s2) = 1 + size s1 + size s2

-- Proj a b: projections from records of type a to records or fields of type b
--           with lazy parallel composition
data Proj :: * -> * -> * where 
    -- Proj: Turn function into field selector
    Proj     :: (a -> b) -> Proj a b

    -- Par : Parallel composition
    Par       :: Proj a b -> Proj c d -> Proj (a, c) (b, d)

-- ext p: functional extension of projection p
ext :: Proj a b -> a -> b
ext (Proj f) x         = f x
ext (Par f1 f2) (x, y) = (ext f1 x, ext f2 y)

-- Pred a : predicate over elements of type a
data Pred :: * -> * where 

    -- Pred: Turn characteristic function into predicate
    Pred      :: (a -> Bool) -> Pred a

    -- TT, FF : always true, resp. false 
    TT        :: Pred a
    FF        :: Pred a
   
    -- And : conjunction
    SAnd      :: Pred a -> Pred a -> Pred a
    
    -- PAnd : componentwise conjunction
    PAnd       :: Pred a -> Pred b -> Pred (a, b)
    
    -- In : equivalence relation on two fields
    In        :: (Proj a k, Proj b k) -> Equiv k -> Pred (a, b)

-- sat p : extension of p as characteristic function
sat :: Pred a -> a -> Bool
sat (Pred f)           = f
sat TT                 = \x -> True
sat FF                 = \x -> False
sat (p1 `SAnd` p2)     = \x -> (sat p1 x && sat p2 x)
sat (p1 `PAnd` p2)      = \(x, y) -> (sat p1 x && sat p2 y)
sat ((f, g) `In` e)    = \(x, y) -> eq e (ext f x) (ext g y)

-- select P s : select (filter) all elements of s that satisfy P
select :: Pred a -> Set a -> Set a

select TT s                       = s
select FF s                       = Set []
select p (Set xs)                 = Set (filter (sat p) xs)
select p (s1 `U` s2)              = select p s1 `U` select p s2

select (Pred f) s@(s1 `X` s2)     = Set (filter f (toList s)) 
  -- explicit product construction, does not occur if f is a field selector!
select (p1 `SAnd` p2) s@(s1 `X` s2) = select p1 (select p2 s)
select (p1 `PAnd` p2) (s1 `X` s2)   = select p1 s1 `X` select p2 s2
select ((f1, f2) `In` e) (s1 `X` s2) = foldr (\b s -> let (xs, ys) = split b
                                                      in (Set xs `X` Set ys) `U` s) empty bs
  where bs = disc e ([(ext f1 r, Left r) | r <- toList s1] ++ 
                     [(ext f2 r, Right r) | r <- toList s2]) -- [[Either a b]]
        split :: [Either a b] -> ([a], [b])
        split [] = ([], [])
        split (v : vs) = let (lefts, rights) = split vs
                         in case v of { Left v' -> (v' : lefts, rights) ;
                                        Right v' -> (lefts, v' : rights) }

-- selectFromWhere : SQL style SELECT on single table (set)
selectFromWhere :: Proj a b -> Set a -> Pred a -> Set b
selectFromWhere p s c = project p (select c s)


collect :: Set a -> Set a
collect s = Set (concat lists) `U` prods
  where (lists, prods) = collect' s ([], empty)
        collect' :: Set a -> ([[a]], Set a) -> ([[a]], Set a)
        collect' (Set xs) (lists, prods) = (xs : lists, prods)
        collect' (s1 `U` s2) (lists, prods) = collect' s1 (collect' s2 (lists, prods))
        collect' s@(s1 `X` s2) (lists, prods) = (lists, s `U` prods)

-- distinct e s: select e-unique values from s
distinct :: Equiv a -> Set a -> Set a
distinct e s = distinct' e (collect s)
  where distinct' :: Equiv a -> Set a -> Set a
        distinct' e (Set xs) = Set (rep e xs)
        distinct' e (s1 `U` s2) = distinct' e s1 `U` distinct' e s2
        distinct' (Pair e1 e2) (s1 `X` s2) = distinct' e1 s1 `X` distinct' e2 s2
        distinct' e s = s

-- project f s : apply field selector f to all elements of s
project :: Proj a b -> Set a -> Set b
project f (Set xs)     = Set (map (ext f) xs)
project f (s1 `U` s2)  = project f s1 `U` project f s2
project (Proj f) s@(s1 `X` s2) = Set (map f (toList s)) -- ouch!
project (Par f1 f2) (s1 `X` s2) = project f1 s1 `X` project f2 s2

-- Example

-- Database schema

data Acct = Acct { 
   acctNum :: Int, 
   acctBranchName :: String, 
   acctBalance :: Int } deriving Show

data Branch = Branch { 
   branchName :: String, 
   branchCity :: String, 
   branchAssets :: Int } deriving Show

data Cust = Cust { 
   custName :: String, 
   custStreet :: String, 
   custCity :: String } deriving Show

data Dep = Dep { 
   depName :: String, 
   depNum :: Int } deriving Show

data Loan = Loan { 
   loanNum :: Int, 
   loanBranchName :: String, 
   loanAmount :: Int } deriving Show

data Borr = Borr { 
   borrName :: String, 
   borrNum :: Int } deriving Show

-- Database state (test data)

accounts :: Set Acct
accounts = Set (repApp 1000 [
      Acct 101 "Downtown"   500,
      Acct 102 "Perryridge" 400,
      Acct 201 "Brighton"   900,
      Acct 215 "Mianus"     700,
      Acct 217 "Brighton"   750,
      Acct 222 "Redwood"    700,
      Acct 305 "Round Hill" 350
      ])

branches :: Set Branch
branches = Set [
      Branch "Brighton"    "Brooklyn"    7100000,
      Branch "Downtown"    "Brooklyn"    9000000,
      Branch "Mianus"      "Horseneck"    400000,
      Branch "North Town"  "Rye"         3700000,
      Branch "Perryridge"  "Horseneck"   1700000,
      Branch "Pownal"      "Bennington"   300000,
      Branch "Redwood"     "Palo Alto"   2100000,
      Branch "Round Hill"  "Horseneck"   8000000
      ]

customers :: Set Cust
customers = Set (repApp 10 [
      Cust "Adams"     "Spring"       "Pittsfield",
      Cust "Brooks"    "Senator"      "Brooklyn"  ,
      Cust "Curry"     "North"        "Rye"       ,
      Cust "Glenn"     "Sand Hill"    "Woodside"  ,
      Cust "Green"     "Walnut"       "Stamford"  ,
      Cust "Hayes"     "Main"         "Harrison"  ,
      Cust "Johnson"   "Alma"         "Palo Alto" ,
      Cust "Jones"     "Main"         "Harrison"  ,
      Cust "Lindsay"   "Park"         "Pittsfield",
      Cust "Smith"     "North"        "Rye"       ,
      Cust "Turner"    "Putnam"       "Stamford"  ,
      Cust "Williams"  "Nassau"       "Princeton" 
      ])

depositors :: Set Dep
depositors = Set (repApp 100 [
      Dep "Hayes"      102,
      Dep "Johnson"    101,
      Dep "Johnson"    201,
      Dep "Jones"      217,
      Dep "Lindsay"    222,
      Dep "Smith"      215,
      Dep "Turner"     305
      ])

loans :: Set Loan
loans = Set [
      Loan 11     "Round Hill"   900,
      Loan 14     "Downtown"    1500,
      Loan 15     "Perryridge"  1500,
      Loan 16     "Perryridge"  1300,
      Loan 17     "Downtown"    1000,
      Loan 23     "Redwood"     2000,
      Loan 93     "Mianus"       500
      ]

borrowers :: Set Borr
borrowers = Set [
      Borr "Adams"      16,
      Borr "Curry"      93,
      Borr "Hayes"      15,
      Borr "Jackson"    14,
      Borr "Jones"      17,
      Borr "Smith"      11,
      Borr "Smith"      23,
      Borr "Williams"   17
      ]


repApp :: Int -> [a] -> [a]
repApp 0 xs = []
repApp (n + 1) xs = xs ++ repApp n xs


-- Queries

bigAccts :: Set Acct
bigAccts = selectFromWhere (Proj id)
           {- FROM -} accounts 
           {- WHERE -} (Pred (\acc -> acctBalance acc > 700))

data DepAcct = DepAcct { depAcctName :: String, 
                         depAcctBranchName :: String, 
                         depAcctBalance :: Int } deriving Show

bigDepAccts :: Set DepAcct
bigDepAccts = selectFromWhere
           (Proj  (\(acct, dep) -> 
              DepAcct (depName dep) (acctBranchName acct) (acctBalance acct)))
           {- FROM -} (bigAccts `X` depositors)
           {- WHERE -} ((Proj acctNum, Proj depNum) `In` eqNat16)

data DepAddrAcct = DepAddrAcct { depAddrAcctName :: String,
                                 depAddrAcctStreet :: String,
                                 depAddrAcctCity :: String,
                                 depAddrAcctBalance :: Int } deriving Show

bigDepAddrAccts :: Set DepAddrAcct 
bigDepAddrAccts = selectFromWhere
           (Proj (\(cust, dep) -> 
             DepAddrAcct (custName cust) (custStreet cust) (custCity cust) (depAcctBalance dep)))
           {- FROM -} (customers `X` bigDepAccts)
           {- WHERE -} ((Proj custName, Proj depAcctName) `In` eqString8)
 

-- Wadler's example, Section 4.2, trwa89

type Ta = Bool
type Tb = Char
type Tc = Char
type Td = Int

data AB = AB {a :: Bool, b :: Char }
data CD = CD {c :: Char, d :: Int}

cond1 :: Pred (AB, CD)
cond1 = (Proj b, Proj c) `In` eqChar8
        `SAnd`
        (TT `PAnd` (Pred (\x -> d x == 99)))

proj1 :: Proj (AB, CD) Ta
proj1 = Proj (\(x, y) -> a x)

q :: Set AB -> Set CD -> Set Ta
q s1 s2 =
 selectFromWhere 
   (Proj (\(x, y) -> a x))
   {- FROM -} (s1 `X` s2)
   {- WHERE -} 
      ((Proj b, Proj c) `In` eqChar8
        `SAnd`
        (TT `PAnd` (Pred (\x -> d x == 99))))
