{-# OPTIONS -fglasgow-exts #-}

----------------------------------------------------------------------------------------
{-| Module      : Relational algebra, with discrimination-based joins
    Copyright   : Fritz Henglein, 2009
    License     : All Rights Reserved

    Maintainer  : Fritz Henglein
    Stability   : Unknown
    Portability : Unknown
-}
----------------------------------------------------------------------------------------
module RelAlg.DB2 where

import RelAlg.Equiv
import RelAlg.Disc

-- Sets, projections and predicates

-- Set a: set of records of type a
data Set a = Set [a] deriving Show

empty :: Set a
empty = Set []

toList :: Set a -> [a]
toList (Set xs) = xs

-- Proj a b: projections from records of type a to records of type b
data Proj a b = Proj (a -> b)

ext :: Proj a b -> a -> b
ext (Proj f) = f

-- Pred a: predicates over records of type a
data Pred a = Pred (a -> Bool) 

sat :: Pred a -> a -> Bool
sat (Pred c) = c

-- Relational algebra operators

-- select p s : set of records from s satisfying p
select :: Pred a -> Set a -> Set a
select (Pred c) (Set xs) = Set (filter c xs)


-- project f s : apply f to each record in s
project :: Proj a b -> Set a -> Set b
project (Proj f) (Set xs) = Set (map f xs)

-- prod s1 s2: cross-product of sets s1 and s2
prod :: Set a -> Set b -> Set (a, b)
prod (Set xs) (Set ys) = Set [(x, y) | x <- xs, y <- ys] -- ouch!


{- joining by discrimination: 
   1. form key-record pairs for the two input tables, tag records to indicate which table they come from
   2. discriminate key-record pairs according to equivalence relation 
   3. return pairs of tables, where each pair contains (obs: this avoids premature product computation)
-}

-- split list of summands into list of Left elements and list of Right elements
split :: [Either a b] -> ([a], [b])
split [] = ([], [])
split (v : vs) = let (lefts, rights) = split vs
                          in case v of { Left v' -> (v' : lefts, rights) ;
                                         Right v' -> (lefts, v' : rights) }

-- join e f1 f2 t1 t2 : binary (equi)join, returning pairs of records (r1, r2) 
--                      such that r1.f1 (e) r2.f2
join :: (Set a, Set b) -> Proj a k -> Equiv k -> Proj b k -> Set (a, b)
join (Set xs, Set ys) (Proj f1) e (Proj f2) =
     Set [ (x, y) | (xs, ys) <- fprods, x <- xs, y <- ys ]
   where bs = disc e ([(f1 x, Left x) | x <- xs] ++ 
                      [(f2 y, Right y) | y <- ys]) -- groups of records with same key
         fprods = map split bs

-- selectFromWhere : SQL style SELECT on single table (set)
selectFromWhere :: Proj a b -> Set a -> Pred a -> Set b
selectFromWhere p s c = project p (select c s)


-- selectFromJoin : SQL style SELECT on two tables,
--                  with join condition given by field selectors and equivalence
selectFromJoin :: Proj (a, b) c -> (Set a, Set b) -> Proj a k -> Equiv k -> Proj b k -> Set c
selectFromJoin p (s1, s2) f1 e f2 =
   project p (join (s1, s2) f1 e f2)

-- Example

-- Database schema

data Acct = Acct { acctNum :: Int, acctBranchName :: String, acctBalance :: Int } deriving Show
data Branch = Branch { branchName :: String, branchCity :: String, branchAssets :: Int } deriving Show
data Cust = Cust { custName :: String, custStreet :: String, custCity :: String } deriving Show
data Dep = Dep { depName :: String, depNum :: Int } deriving Show
data Loan = Loan { loanNum :: Int, loanBranchName :: String, loanAmount :: Int } deriving Show
data Borr = Borr { borrName :: String, borrNum :: Int } deriving Show

-- Database state (test data)

accounts :: Set Acct
accounts = Set [
      Acct 101 "Downtown"   500,
      Acct 102 "Perryridge" 400,
      Acct 201 "Brighton"   900,
      Acct 215 "Mianus"     700,
      Acct 217 "Brighton"   750,
      Acct 222 "Redwood"    700,
      Acct 305 "Round Hill" 350
      ]

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
customers = Set [
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
      ]

depositors :: Set Dep
depositors = Set [
      Dep "Hayes"      102,
      Dep "Johnson"    101,
      Dep "Johnson"    201,
      Dep "Jones"      217,
      Dep "Lindsay"    222,
      Dep "Smith"      215,
      Dep "Turner"     305
      ]

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

-- Queries

bigAccts :: Set Acct
bigAccts = selectFromWhere (Proj id)
           {- FROM -} accounts 
           {- WHERE -} (Pred (\acc -> acctBalance acc > 700))

data DepAcct = DepAcct { depAcctName :: String, 
                         depAcctBranchName :: String, 
                         depAcctBalance :: Int } deriving Show

bigDepAccts :: Set DepAcct
bigDepAccts = selectFromJoin
           (Proj  (\(acct, dep) -> DepAcct (depName dep) (acctBranchName acct) (acctBalance acct)))
           {- FROM -} (bigAccts, depositors)
           {- WHERE -} (Proj acctNum) eqNat16 (Proj depNum)

data DepAddrAcct = DepAddrAcct { depAddrAcctName :: String,
                                 depAddrAcctStreet :: String,
                                 depAddrAcctCity :: String,
                                 depAddrAcctBalance :: Int } deriving Show

bigDepAddrAccts :: Set DepAddrAcct 
bigDepAddrAccts = selectFromJoin
           (Proj (\(cust, dep) -> 
             DepAddrAcct (custName cust) (custStreet cust) (custCity cust) (depAcctBalance dep)))
           {- FROM -} (customers, bigDepAccts)
           {- WHERE -} (Proj custName) eqString8 (Proj depAcctName)
