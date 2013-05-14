module Test.TestData where

import Data.Map as Map

import Formula.SyntaxGeo
import Tools.GeoUnification
import Chase.Problem.Observation
import qualified Chase.Problem.Model as Model(empty)
import Chase.Problem.Structures
import qualified CC.CC as CC
import Chase.Problem.IModel

--Formulas
fmlaTru = Tru
fmlaFls = Fls
fmla1 = parseFormula "P(x)"
fmla2 = parseFormula "P(x) & Q(y)"
fmla3 = parseFormula "P(x) | Q(y)"
fmla4 = parseFormula "exists x.P(x)"
fmla5 = parseFormula "(P(x) & Q(y)) | (exists z.R(x,z) & Q(y))"
fmla6 = parseFormula "P(x) & (Q(y) | R(x,y))"
fmla7 = parseFormula "f(x) = g(y)"
fmla8 = parseFormula "f(x) = g(y) & P(a())"

-- Terms
atm1 = let Atm atm = parseFormula "P(x)" in atm
trm1 = parseTerm "P(x)"
atm2 = let Atm atm = parseFormula "Q(y)" in atm
atm3 = let Atm atm = parseFormula "R(x,z)" in atm
trm4 = parseTerm "f(x)"
trm5 = parseTerm "g(y)"
atm6 = let Atm atm = parseFormula "P(a())" in atm
trm6 = parseTerm "P(a())"
atm7 = let Atm atm = parseFormula "Q(b())" in atm
trm8 = parseTerm "f(a())"
trm9 = parseTerm "a()"
atm10 = let Atm atm = parseFormula "P(f(x))" in atm
atm11 = let Atm atm = parseFormula "Q(x)" in atm
atm12 = let Atm atm = parseFormula "R(y,z)" in atm
atm13 = let Atm atm = parseFormula "R(x,x)" in atm
atm14 = let Atm atm = parseFormula "S(x)" in atm
atm15 = let Atm atm = parseFormula "S(b)" in atm
atm16 = let Atm atm = parseFormula "R(y,y)" in atm
atm17 = let Atm atm = parseFormula "R(x,y)" in atm
trm18 = parseTerm "g(a())"
trm19 = Elm "const0"
atm19 = let Atm atm = parseFormula "const0()" in atm
atm20 = let Atm atm = parseFormula "R(f(a()), g(f(a())))" in atm
trm21 = parseTerm "a()"
trm22 = parseTerm "f(a())"
trm23 = parseTerm "g(f(a()))"
atm24 = let Atm atm = parseFormula "P(f(a()))" in atm
trm24 = parseTerm "P(f(a()))"
trm25 = parseTerm "b"
trm26 = Elm "const0" -- const0()
trm27 = Fn "P" [trm26] -- P(const0())
trm28 = Elm "const1" -- const1()
trm29 = Fn "f" [trm26] -- f(const0())
trm30 = parseTerm "b()" -- b()
trm31 = Elm "const2" -- const2()
trm32 = Elm "const3" -- const3()
trm33 = Fn "Q" [trm32] -- Q(const3())
trm34 = Elm "const4" -- const4()
trm35 = Elm "const5" -- const5()
trm36 = Elm "const6" -- const6()
trm37 = Fn "g" [trm26] -- g(const0())
atm38 = R "P" [Elm "a1"] -- P(a1())
trm39 = parseTerm "x" -- x
trm40 = Elm "a1" -- a1()
trm41 = Elm "a2" -- a1()

-- Observations
obs0 = Den truth -- True
obs1 = Fct atm1 -- P(x)
obs2 = Fct atm2 -- Q(y)
obs3 = Fct atm3 -- R(x,z)
obs4 = Eql trm4 trm5 -- f(x) = g(y)
obs5 = Fct atm6 -- P(a())
obs6 = Fct atm7 -- Q(b())
obs7 = Eql trm8 trm9 -- f(a()) = a()
obs8 = Fct atm10 -- P(f(x))
obs9 = Fct atm11 -- Q(x)
obs10 = Fct atm12 -- R(y,z)
obs11 = Fct atm13 -- R(x,x)
obs12 = Fct atm14 -- S(x)
obs13 = Fct atm15 -- S(b)
obs14 = Fct atm16 -- R(y,y)
obs15 = Fct atm17 -- R(x,y)
obs16 = Eql trm8 trm18 -- f(a()) = g(a())
obs17 = Den trm19 -- const0
obs18 = Fct atm20 -- R(f(a()), g(f(a())))
obs19 = Den trm21 -- a()
obs20 = Den trm22 -- f(a())
obs21 = Den trm23 -- g(f(a()))
obs22 = Fct atm24 -- P(f(a()))
obs23 = Den trm18 -- g(a())
obs24 = Fct atm38 -- P(a1())
obs25 = Eql trm39 trm8 -- x = f(a())
obs26 = Eql trm40 trm8 -- a1() = f(a())
obs27 = Eql trm41 trm8 -- a2() = f(a())

-- Sequents
sqnt1 = parseSequent "Truth => Falsehood"
sqnt2 = parseSequent "P(x)"
sqnt3 = parseSequent "P(x) => Falsehood"
sqnt4 = parseSequent "P(a()) => Q(b())"
sqnt5 = parseSequent "P(a()) & Q(y) => (R(x,z) & P(x)) | Q(b())"
sqnt6 = parseSequent "Q(b()) => f(a()) = a()"
sqnt7 = parseSequent "P(f(x)) => Q(x)"
sqnt8 = parseSequent "P(a())"
sqnt9 = parseSequent "Truth => exists y.exists z.R(y,z)"
sqnt10 = parseSequent "Truth => exists x.Q(x)"
sqnt11 = parseSequent "Q(x) => R(x,x) | S(x)"
sqnt12 = parseSequent "Q(x) => R(x,x)"
sqnt13 = parseSequent "R(y,z) => exists b.S(b)"
sqnt14 = parseSequent "R(y,y) => exists x.Q(x) & R(x,y)"

-- Frames
frm1 = Frame (ID 1) [] [] [] -- Truth => Falsehood
frm2 = Frame (ID 2) [] [[obs1]] ["x"] -- P(x)
frm3 = Frame (ID 3) [obs1] [] ["x"] -- P(x) => Falsehood
frm4 = Frame (ID 4) [obs5] [[obs6]] [] -- P(a()) => Q(b())
frm5 = Frame (ID 5) [obs5, obs2] [[obs3, obs1], [obs6]] ["x", "y", "z"]
       -- P(a()) & Q(y) => (R(x,z) & P(x)) | Q(b())
frm6 = Frame (ID 6) [obs6] [[obs7]] [] -- Q(b()) => f(a()) = a()
frm7 = Frame (ID 7) [obs8] [[obs9]] ["x"] -- P(f(x)) => Q(x)
frm8 = Frame (ID 8) [] [[obs5]] [] -- P(a())
frm9 = Frame (ID 9) [] [[obs10]] [] -- R(y,z)
frm10 = Frame (ID 10) [] [[obs9]] [] -- Q(x)
frm11 = Frame (ID 11) [obs9] [[obs11], [obs12]] ["x"]
        -- Q(x) => R(x,x) | S(x)
frm12 = Frame (ID 12) [obs9] [[obs11]] ["x"] -- Q(x) => R(x,x)
frm13 = Frame (ID 13) [obs10] [[obs13]] ["y", "z"]
        -- R(y,z) => S(b)
frm14 = Frame (ID 14) [obs14] [[obs9, obs15]] ["y"]
        -- R(y,y) => Q(x) & R(x,y)
frm15 = Frame (ID 15) [] [[obs7]] [] -- f(a()) = a()
frm16 = Frame (ID 16) [] [[obs5, obs6]] [] -- P(a()) & Q(b())
frm17 = Frame (ID 17) [] [[obs6]] [] -- Q(b())
frm18 = Frame (ID 18) [] [[obs5], [obs6]] [] -- P(a()) | Q(b())
frm19 = Frame (ID 19) [] [[obs1]] [] -- exists x. P(x)
frm20 = Frame (ID 20) [] [[obs1, obs25]] [] 
        -- exists x.(P(x) & (x = f(a())))
frm21 = Frame (ID 21) [] [[obs1], [obs25]] [] 
        -- exists x.P(x) | exists x.(x = f(a()))
frm22 = Frame (ID 22) [] [[obs1], [obs22, obs6]] [] 
        -- exists x.P(x) | P(f(a())) & Q(b()))

-- Theories
thy1 = [sqnt8, sqnt4, sqnt6, sqnt7] --thyf12 from test cases
thy2 = [sqnt1]
thy3 = [sqnt1, sqnt2, sqnt3, sqnt4, sqnt5]
thy4 = [sqnt9]
thy5 = [sqnt10, sqnt11, sqnt12, sqnt13, sqnt14]
       --thy7 from test cases

-- Frame Lists
frmlst1 = [frm8, frm4, frm6, frm7] --thy1
frmlst2 = [frm1] --thy2
frmlst3 = [frm1, frm2, frm3, frm4, frm5] --thy3
frmlst4 = [frm9] --thy 4
frmlst5 = [frm10, frm11, frm12, frm13, frm14] --thy5

-- Symbol Maps
symMap1 = Map.fromList[("P",[(ID 4,0),(ID 7,0)]),
                       ("Q",[(ID 6,0)]),
                       ("a",[(ID 4,0)]),
                       ("b",[(ID 6,0)]),
                       ("f",[(ID 7,0)])] :: SymbolMap -- frmlst1
symMap2 = Map.empty :: SymbolMap
symMap3 = Map.fromList[("P",[(ID 3,0),(ID 4,0),(ID 5,0)]),
                       ("Q",[(ID 5,1)]),
                       ("a",[(ID 4,0),(ID 5,0)])] :: SymbolMap 
                       --frmlst3
symMap4 = Map.fromList [("Q",[(ID 11,0),(ID 12,0)]),
                        ("R",[(ID 13,0),(ID 14,0)])] :: SymbolMap
                       --frmlst4

-- Problems
frm8_for_prob1 = Frame (ID 1) [] [[obs5]] [] -- P(a())
frm4_for_prob1 = Frame (ID 2) [obs5] [[obs6]] [] -- P(a()) => Q(b())
frm6_for_prob1 = Frame (ID 3) [obs6] [[obs7]] [] -- Q(b()) => f(a()) = a()
frm7_for_prob1 = Frame (ID 4) [obs8] [[obs9]] ["x"] -- P(f(x)) => Q(x)

frmlst1_for_prob1 = [frm8_for_prob1, frm4_for_prob1, frm6_for_prob1,
                     frm7_for_prob1]

symMap1_for_prob1 = Map.fromList[("P",[(ID 2,0),(ID 4,0)]),
                                 ("Q",[(ID 3,0)]),
                                 ("a",[(ID 2,0)]),
                                 ("b",[(ID 3,0)]),
                                 ("f",[(ID 4,0)])] :: SymbolMap -- frmlst1

prob1 = Problem frmlst1_for_prob1 Model.empty (Queue []) 
        symMap1_for_prob1 (ID 4) 0

prob1_1 = Problem fs m (Queue [obs1]) s i c
    where (Problem fs m _ s i c) = prob1
-------------------------------------------------
frm9_for_prob2 = Frame (ID 1) [] [[obs10]] []
frmlst4_for_prob2 = [frm9_for_prob2]

prob2 = Problem frmlst4_for_prob2 Model.empty (Queue [])
        symMap2 (ID 1) 0

-------------------------------------------------
symMap4_for_prob3 = Map.fromList [("Q",[(ID 2,0),(ID 3,0)]),
                                  ("R",[(ID 4,0),(ID 5,0)])] :: SymbolMap
frm10_for_prob3 = Frame (ID 1) [] [[obs9]] []
frm11_for_prob3 = Frame (ID 2) [obs9] [[obs11], [obs12]] ["x"]
frm12_for_prob3 = Frame (ID 3) [obs9] [[obs11]] ["x"]
frm13_for_prob3 = Frame (ID 4) [obs10] [[obs13]] ["y", "z"]
frm14_for_prob3 = Frame (ID 5) [obs14] [[obs9, obs15]] ["y"]

frmlst_for_prob3 = [frm10_for_prob3, frm11_for_prob3, 
                    frm12_for_prob3, frm13_for_prob3, 
                    frm14_for_prob3]
prob3 = Problem frmlst_for_prob3 Model.empty (Queue [])
        symMap4_for_prob3 (ID 5) 0
-------------------------------------------------
-- CC Equations
cceq1 = CC.Eql trm6 truth -- P(a) = True
cceq2 = CC.Eql trm8 trm9 -- f(a) = a
-------------------------------------------------
-- Models
mdl0 = Model.empty
-------------------------
trs1 = [CC.RW trm27 truth,  -- P(const0()) -> True
        CC.RW trm21 trm26,  -- a() -> const0()
        CC.RW trm28 truth]  -- const1() -> True

dom1 = [trm26, trm28, truth] -- const0(), const1(), True
mdl1 = Model trs1 dom1
-------------------------
trs2 = [CC.RW trm28 trm26, -- const1() -> const0()
        CC.RW trm21 trm26, -- a() -> const0()
        CC.RW trm29 trm26] -- f(const0()) -> const0()

dom2 = [trm26, trm28] -- const0() const1()
mdl2 = Model trs2 dom2
-------------------------
trs3 = [CC.RW trm21 trm26, -- a() -> const0()
        CC.RW trm27 truth, -- P(const0() -> True
        CC.RW trm30 trm32, -- b() -> const3()
        CC.RW trm33 truth, -- Q(const3()) -> True
        CC.RW trm28 truth, -- const1() -> True
        CC.RW trm34 truth, -- const4() -> True
        CC.RW trm35 trm26, -- const5() -> const0()
        CC.RW trm29 trm26] -- f(const0()) -> const0()

dom3 = [trm26, trm28, trm32, trm34, trm35, truth] 
       -- const0(), const1(), const3(), const4(), const5()
mdl3 = Model trs3 dom3
-------------------------
trs4 = [CC.RW trm21 trm26, -- a() -> const0()
        CC.RW trm27 truth, -- P(const0()) -> True
        CC.RW trm30 trm32, -- b() -> const3()
        CC.RW trm33 truth, -- Q(const3()) -> True
        CC.RW trm28 truth, -- const1() -> True
        CC.RW trm34 truth, -- const4() -> True
        CC.RW trm35 trm26, -- const5() -> const0()
        CC.RW trm36 trm26, -- const6() -> const0()
        CC.RW trm29 trm26, -- f(const0()) -> const0()
        CC.RW trm37 trm26] -- g(const0()) -> const0()

dom4 = [trm26, trm28, trm32, trm34, trm35, trm36, truth]
       -- const0(), const1(), const3(), const4(), const5(), const6(), True
mdl4 = Model trs4 dom4
-------------------------
trs5 = [CC.RW trm21 trm26, -- a() -> const0()
        CC.RW trm29 trm26] -- f(const0()) -> const0()

dom5 = [trm26, truth] 
       -- const0(), True
mdl5 = Model trs5 dom5
-------------------------
-- Substitutions
sub0 = Map.empty
sub1 = Map.fromList [("x", trm25)] -- x -> b
sub2 = Map.fromList [("x", trm21)] -- x -> a()
sub3 = Map.fromList [("x", trm22)] -- x -> f(a())
sub4 = Map.fromList [("x", trm22), ("z", trm23)] -- x -> f(a()), z -> g(f(a()))
sub5 = Map.fromList [("y", trm22)] -- y -> f(a())