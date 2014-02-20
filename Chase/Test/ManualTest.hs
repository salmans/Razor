{- Time-stamp: <2012-11-19 14:39:51 Salman Saghafi>
   This module contains a set of simple formulas and theories only
   for manual test purposes.
-}

module Chase.Test.ManualTest where

import Chase
import Chase.WeaklyAcyclic.WeaklyAcyclic
import Chase.CC.CC
--import WeaklyAcyclic
--TEST
-- Sequents:
test_fmla1 = "Truth => P(a(),b())"
test_fmla1_5 = "Truth => P(f(a()),b())"
test_fmla1_6 = "Truth => P(a(), b()) | P(c(), d())"

test_fmla2 = "Truth => T(c())"

test_fmla3 = "(P(x,y) & Q(y,z)) => R(x,z)"
test_fmla3_1 = "(P(x,y) & Q(y,z)) => exists w.R(w,z)"
test_fmla3_2 = "(P(x,y) & Q(y,z)) => exists v.exists w.R(v,w)"
test_fmla3_3 = "(P(x,y) & Q(y,z)) => (R(x,z) & S(x))"
test_fmla3_4 = "(P(x,y) & Q(y,z)) => (R(x,z) & (exists v.exists w.P(v,w)))"
test_fmla3_5 = "(P(f(x),y) & Q(y,z)) => R(x,z)"

test_fmla4 = "(R(x,y) & S(x)) => Falsehood"

test_fmla5 = "(P(x,y) & T(z)) => Q(y,z)"
test_fmla5_5 = "(P(f(x),y) & T(z)) => Q(y,z)"

test_fmla6 = "Truth => P(c(), d())"

test_fmla7 = "Truth => S(a())"

test_fmla8 = "P(x,y) => exists z.Q(x,z)"

test_fmla9 = "Q(x,y) => exists z.P(z,y)"

test_fmla10 = "Truth => exists x. exists y.R(x,y)"

test_fmla11 = "R(x,y) => exists z. R(x,z)"

test_fmla12 = "R(x,y) => R(y,x)"

-- Geometric sets:
test_fmlas1 = [test_fmla1, test_fmla2, test_fmla3, test_fmla4,
                  test_fmla5] -- SAT
test_fmlas1_1 = [test_fmla1, test_fmla2, test_fmla3_1, test_fmla4,
                  test_fmla5] -- SAT
test_fmlas1_2 = [test_fmla1, test_fmla2, test_fmla3_2, test_fmla4,
                  test_fmla5] -- SAT
test_fmlas1_3 = [test_fmla1, test_fmla2, test_fmla3_3, test_fmla4,
                  test_fmla5] -- UNSAT
test_fmlas1_4 = [test_fmla1, test_fmla2, test_fmla3_4, test_fmla4,
                  test_fmla5] -- SAT
test_fmlas1_5 = [test_fmla1_5, test_fmla2, test_fmla3_5, test_fmla4,
                  test_fmla5_5] -- SAT
test_fmlas1_6 = [test_fmla1_6, test_fmla2, test_fmla3, test_fmla4,
                  test_fmla5] -- SAT

test_fmlas2 = test_fmla6:test_fmlas1 -- SAT
test_fmlas2_1 = test_fmla6:test_fmlas1_1 -- SAT
test_fmlas2_2 = test_fmla6:test_fmlas1_2 -- SAT

test_fmlas3 = test_fmla7:test_fmlas1 -- UNSAT

test_fmlas4 = [test_fmla1, test_fmla8, test_fmla9] -- NON-TERMINATING

test_fmlas5 = [test_fmla10, test_fmla11, test_fmla12]