{- This file is part of Razor.

  Razor is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Razor is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Razor.  If not, see <http://www.gnu.org/licenses/>.

  Module      : Syntax.Test.TestData
  Description : Test data for terms and formulas
  Maintainer  : Salman Saghafi <salmans@wpi.edu>, Ryan Danas <ryandanas@wpi.edu>
-}

module Syntax.Test.TestData where

-- Standard
import qualified Data.Map as Map

-- Syntax
import Syntax.ITerm
import Syntax.IGeometric
import qualified Syntax.IFirstOrder as Fol

-- shortcuts
var   = Variable
cons  = Constant
elm   = Element
elemPred x = Atm $ Rel "@Element" [Var $ var x]
consTerm = Cons . Constant
elemTerm = Elem . Element

text_varX   = "x"
text_varY   = "y"
text_varVar = "var"
text_consC  = "'c"
text_consConst  = "'const"
text_termSimple1  = "f(x, 'c)"
text_termComplex1 = "f(var, 'const, g(x, 'c))"

text_truth  = "Truth"
text_falsehood = "Falsehood"
text_atomSimple1 = "R(x)"
text_atomSimple2 = "R(x, 'c)"
text_atomComplex1 = "R(f(var, 'const, g(x, 'c)))"
text_atomComplex2 = "R(f(x,'c), f(var, 'const, g(x, 'c)))"
text_fmlaPxAndQy  = "P(x) & Q(y)"
text_fmlaPxOrQy   = "P(x) | Q(y)"
text_fmlaExists1  = "exists x. P(x, y)"
text_fmlaExists2  = "exists func x. P(x, y)"
text_fmlaExistsPx = "exists x. P(x)"
text_PxAndQxy        = "P(x) & Q(x, y)"

text_seqTriv1 = "Truth => Falsehood"
text_seqTriv2 = "Falsehood"
text_seqTriv3 = "~Truth"
text_seqSimple1 = "P(x) => Q(x)"
text_seqSimple2 = "P(x)"
text_seqSimple3 = "~P(x)"
text_seqComplex1 = "P(x) & Q(y) => P(x) | Q(y)"
text_seqComplex2 = "P(x) | Q(y) => P(x) & Q(y)"
text_seqComplex3 = "P(x) & Q(y) => (exists x . P(x, y)) | P(x) & Q(y)"

term_varX = Var $ Variable text_varX
term_varY = Var $ Variable text_varY
term_varVar = Var $ Variable text_varVar
term_consA = Cons $ Constant "a"
term_consB = Cons $ Constant "b"
term_consC = Cons $ Constant "c"
term_elmE  = Elem $ Element "e"
term_elemE0 = elemTerm "e#0"
term_elemE1 = elemTerm "e#1"
term_elemE2 = elemTerm "e#2"
term_consConst = Cons $ Constant "const"
term_fnF       = Fn "f" []
term_fnFbc     = Fn "f" [term_consB, term_consC]
term_fnFafnGb  = Fn "f" [term_consA, Fn "g" [term_consB]]
term_fnFbxbc   = Fn "f" [term_consB, term_varX, term_consB, term_consC]
term_fnFxc     = Fn "f" [term_varX, term_consC]
term_fnFxcxy   = Fn "f" [term_varX, term_consC, term_varX, term_varY]
term_termSimple1 = Fn "f" [term_varX, term_consC]
term_termComplex1 = Fn "f" [ term_varVar
                           , term_consConst
                           , Fn "g" [term_varX, term_consC]]


atm_P        = Rel "P" []
atm_Pa       = Rel "P" [term_consA]
atm_Pc       = Rel "P" [term_consC]
atm_Px       = Rel "P" [term_varX]
atm_Pxcx     = Rel "P" [term_varX, term_consC, term_varX]
atm_Pcxc     = Rel "P" [term_consC, term_varX, term_consC]
atm_Complex2 = Rel "R" [term_termSimple1, term_termComplex1]
atm_Fx       = FnRel "F" [term_varX]
atm_Fc       = FnRel "F" [term_consC]
atm_Complex3 = FnRel "F" [term_termComplex1]
atm_Complex4 = Rel "P" [term_fnFafnGb]
atm_Complex5 = FnRel "FR" [term_fnFafnGb]
atm_ElemE0   = Rel "@Element" [term_elemE0]
atm_ElemE1   = Rel "@Element" [term_elemE1]
atm_ElemE2   = Rel "@Element" [term_elemE2]
atm_EqlE0E0  = Rel "=" [term_elemE0, term_elemE0]
atm_EqlE1E1  = Rel "=" [term_elemE1, term_elemE1]
atm_EqlE2E2  = Rel "=" [term_elemE2, term_elemE2]
atm_Pe0      = Rel "P" [term_elemE0]
atm_Pe1      = Rel "P" [term_elemE1]
atm_Qe0      = Rel "Q" [term_elemE0]
atm_Qe1      = Rel "Q" [term_elemE1]
atm_Re0      = Rel "R" [term_elemE0]
atm_Re1      = Rel "R" [term_elemE1]
atm_Se0      = Rel "S" [term_elemE0]
atm_Se1      = Rel "S" [term_elemE1]
atm_Ue0      = Rel "U" [term_elemE0]
atm_Ue1      = Rel "U" [term_elemE1]
atm_Qe0e0    = Rel "Q" [term_elemE0, term_elemE0]
atm_Qe0e1    = Rel "Q" [term_elemE0, term_elemE1]
atm_Re0e0    = Rel "R" [term_elemE0, term_elemE0]
atm_Re0e1    = Rel "R" [term_elemE0, term_elemE1]
atm_Re2e1    = Rel "R" [term_elemE2, term_elemE1]

fmla_truth = Tru
fmla_falsehood = Fls
fmla_Pa = Atm $ Rel "P" [term_consA]
fmla_Px = Atm $ Rel "P" [term_varX]
fmla_Pax = Atm $ Rel "P" [term_consA, term_varX]
fmla_Py = Atm $ Rel "P" [term_varY]
fmla_Qb = Atm $ Rel "Q" [term_consB]
fmla_Qc = Atm $ Rel "Q" [term_consC]
fmla_Qx = Atm $ Rel "Q" [term_varX]
fmla_Qy = Atm $ Rel "Q" [term_varY]
fmla_Rx = Atm $ Rel "R" [term_varX]
fmla_Rxy = Atm $ Rel "R" [term_varX, term_varY]
fmla_Fx = Atm $ FnRel "F" [term_varX]
fmla_Pxx = Atm $ Rel "P" [term_varX, term_varX]
fmla_Pxcx = Atm $ Rel "P" [term_varX, term_consC, term_varX]
fmla_Pcxc = Atm $ Rel "P" [term_consC, term_varX, term_consC]
fmla_Rxc  = Atm $ Rel "R" [Var (Variable "x"), Cons (Constant "c")]
fmla_atomComplex1 = Atm $ Rel "R" [term_termComplex1]
fmla_atomComplex2 = Atm $ Rel "R" [term_termSimple1, term_termComplex1]
fmla_atomComplex3 = Atm $ FnRel "F" [term_termComplex1]
fmla_PxAndQy    = And fmla_Px fmla_Qy
fmla_PaAndQb    = And fmla_Pa fmla_Qb
fmla_PxAndQx     = And fmla_Px fmla_Qx
fmla_PxAndQxAndRx = And fmla_Px (And fmla_Qx fmla_Rx) 
fmla_PyAndQxAndRxy = And fmla_Py (And fmla_Qx fmla_Rxy)
fmla_PaxAndRxy = And fmla_Pax fmla_Rxy
fmla_PaOrQb      = Or fmla_Pa fmla_Qb
fmla_PaOrQc      = Or fmla_Pa fmla_Qc
fmla_PxOrQy      = Or fmla_Px fmla_Qy
fmla_exists1  = Exists Nothing (Variable "x") 
                (Atm (Rel "P" [term_varX, term_varY]))
fmla_exists1_R  = Exists (Just "exists#0") (Variable "x") 
                  (Atm (Rel "P" [term_varX, term_varY]))
fmla_exists2  = Exists (Just "func") (Variable "x") 
                (Atm (Rel "P" [term_varX, term_varY]))
fmla_existsPx   = Exists Nothing (Variable "x") (Atm (Rel "P" [term_varX]))
fmla_existsPxa  = Exists Nothing (Variable "x") 
                  (Atm (Rel "P" [term_varX, term_consA]))
fmla_existsPx_R = Exists (Just "exists#0") (Variable "x") 
                  (Atm (Rel "P" [term_varX]))
fmla_exists3    = parseFormula "exists sk1 x. exists sk2 y. P(x, y)"
fmla_exists4    = parseFormula "(exists sk1 z. P(z)) & (exists sk2 y. Q(x, y))"
fmla_exists5    = parseFormula "P(z) | (exists sk1 y. Q(x, y))"
fmla_exists6    = parseFormula "exists sk1 x. P(x)"

fmla_PxAndQxy    = parseFormula text_PxAndQxy
fmla_PfcxAndQxgyz  = parseFormula "P(f('c, x)) & Q(x, g(y, z))" 
fmla_PfcxOrQxgyz   = parseFormula "P(f('c, x)) | Q(x, g(y, z))" 
fmla_existsQxfyz   = parseFormula "exists x. Q(x, f(y, z))" 

seq_Triv = Sequent Tru Fls
seq_Simple1 = Sequent fmla_Px (Atm (Rel "Q" [term_varX]))
seq_Simple2 = Sequent Tru fmla_Px
seq_Simple2_El = Sequent (elemPred "x") fmla_Px
seq_Simple3 = Sequent fmla_Px Fls
seq_Simple4 = Sequent fmla_Px fmla_PxAndQy
seq_Simple4_El = Sequent (And (elemPred "y") fmla_Px) fmla_PxAndQy
seq_Simple5 = Sequent fmla_Px fmla_existsPx
seq_Simple5_El = Sequent fmla_Px (Exists Nothing (var "x") 
                                  (And (elemPred "x") fmla_Px))
seq_Simple6    = Sequent Tru (And fmla_PxAndQx fmla_existsPx)
seq_Simple6_El = Sequent (elemPred "x") 
                 (And fmla_PxAndQx
                          (Exists Nothing (var "x") 
                           (And (elemPred "x") fmla_Px )))
seq_Simple7  = Sequent Tru fmla_Pa
seq_Simple8  = Sequent fmla_Pa Fls
seq_Complex1 = Sequent fmla_PxAndQy fmla_PxOrQy
seq_Complex2 = Sequent fmla_PxOrQy fmla_PxAndQy
seq_Complex3 = Sequent fmla_PxAndQy (Or fmla_exists1 fmla_PxAndQy)
seq_Complex3_R = Sequent fmla_PxAndQy (Or fmla_exists1_R fmla_PxAndQy)
seq_Complex4 = Sequent fmla_Px fmla_exists1
seq_Complex5 = Sequent fmla_PaAndQb fmla_PaOrQc
seq_Complex6 = Sequent fmla_PaAndQb (Or fmla_exists1 fmla_PaOrQc)

text_seq1 = "P(x) => Q(f(x))"
seq_seq1 = parseSequent text_seq1
seq_seq1_R = Sequent fmla_Px 
             $ Lone (Just "f#1") (var "v#0")
               (And (Atm $ FnRel "f" [Var (var "x"), Var (var "v#0")])
                        (Atm $ Rel "Q" [Var (var "v#0")]))
               (Atm $ FnRel "f" [Var (var "x"), Var (var "v#0")])
text_seq2 = "P(x) => Q(f(x), g(y))"
seq_seq2 = parseSequent text_seq2
seq_seq2_R = Sequent fmla_Px 
             $ Lone (Just "g#3") (var "v#2") 
                   (Lone (Just "f#1") (var "v#0")
                   (And (Atm $ FnRel "f" [Var (var "x"), Var (var "v#0")])
                         $ And (Atm $ FnRel "g" [ Var (var "y")
                                                , Var (var "v#2")])
                               (Atm $ Rel "Q" [ Var (var "v#0")
                                              , Var (var "v#2")]))
                   (Atm $ FnRel "f" [Var (var "x"), Var (var "v#0")]))
                   (Atm $ FnRel "g" [Var (var "y"), Var (var "v#2")])
text_seq3 = "P(x) => Q(f())"
seq_seq3  = parseSequent text_seq3

text_seq4 = "P(x) => Q('c)"
seq_seq4  = parseSequent text_seq4

text_seq5 = "Q(f(x)) => P(x)"
seq_seq5  = parseSequent text_seq5
seq_seq5_R = Sequent
             ( Lone (Just "f#1") (var "v#0")
               (And (Atm $ FnRel "f" [Var (var "x"), Var (var "v#0")])
                        (Atm $ Rel "Q" [Var (var "v#0")]))
               (Atm $ FnRel "f" [Var (var "x"), Var (var "v#0")]))
             fmla_Px

seq_seq6   = parseSequent "P(f(x)) => Q(x) & P(g(x, 'c))"

seq_seqExists1 = parseSequent "exists sk1 x. P(x)"
seq_seqExists2 = parseSequent "exists sk1 x. P(x) => Falsehood"
seq_seqExists3 = parseSequent 
                 "exists sk1 x. P(x) => exists sk2 x. exists sk3 y. Q(x, y)"

seq_integ0 = Sequent (And (Atm $ FnRel "f" [ Var (var "y")])
                          (Atm $ FnRel "f" [ Var (var "y'")]))
             (parseFormula "y = y'")
seq_integ1 = Sequent (And (Atm $ FnRel "f" [ Var (var "x1")
                                           , Var (var "y")])
                          (Atm $ FnRel "f" [ Var (var "x1")
                                           , Var (var "y'")]))
             (parseFormula "y = y'")
seq_integ2 = Sequent (And (Atm $ FnRel "f" [ Var (var "x1")
                                           , Var (var "x2")
                                           , Var (var "y")])
                          (Atm $ FnRel "f" [ Var (var "x1")
                                           , Var (var "x2")
                                           , Var (var "y'")]))
             (parseFormula "y = y'")
seq_integ3 = Sequent (And (Atm $ FnRel "f" [ Var (var "x1")
                                           , Var (var "x2")
                                           , Var (var "x3")
                                           , Var (var "y")])
                          (Atm $ FnRel "f" [ Var (var "x1")
                                           , Var (var "x2")
                                           , Var (var "x3")
                                           , Var (var "y'")]))
             (parseFormula "y = y'")


exSub_sk1Toa = Map.singleton "sk1" (consTerm "a")
exSub_sk1Toask2Tob = Map.fromList [("sk1", consTerm "a"), ("sk2", consTerm "b")]
--------------------------------------------------------------------------------
-- First-Order data:
foa_P         = Fol.Rel "P" []
foa_Px        = Fol.Rel "P" [term_varX]
foa_Pc        = Fol.Rel "P" [term_consC]
foa_Pxcx      = Fol.Rel "P" [term_varX, term_consC, term_varX]
foa_Pcxc      = Fol.Rel "P" [term_consC, term_varX, term_consC]
foa_Complex2  = Fol.Rel "R" [term_termSimple1, term_termComplex1]
foa_Fc        = Fol.FnRel "F" [term_consC]
foa_Fx        = Fol.FnRel "F" [term_varX]
foa_Complex3  = Fol.FnRel "F" [term_termComplex1]

fof_truth     = Fol.Tru
fof_falsehood = Fol.Fls
fof_Pa        = Fol.Atm $ Fol.Rel "P" [term_consA]
fof_Px        = Fol.Atm $ Fol.Rel "P" [term_varX]
fof_Qb        = Fol.Atm $ Fol.Rel "Q" [term_consB]
fof_Qy        = Fol.Atm $ Fol.Rel "Q" [term_varY]
fof_Rx        = Fol.Atm $ Fol.Rel "R" [term_varX]
fof_Rxc       = Fol.Atm $ Fol.Rel "R" [Var (Variable "x"), Cons (Constant "c")]
fof_atomComplex1 = Fol.Atm $ Fol.Rel "R" [term_termComplex1]
fof_atomComplex2 = Fol.Atm $ Fol.Rel "R" [term_termSimple1, term_termComplex1]
fof_PaAndQb      = Fol.And fof_Pa fof_Qb
fof_PxAndQy      = Fol.And fof_Px fof_Qy
fof_PaOrQb       = Fol.Or fof_Pa fof_Qb
fof_PxOrQy       = Fol.Or fof_Px fof_Qy
fof_PxImpQy      = Fol.Imp fof_Px fof_Qy
fof_PxIffQy      = Fol.Iff fof_Px fof_Qy
fof_exists1      = Fol.Exists Nothing (Variable "x") 
                   (Fol.Atm (Fol.Rel "P" [term_varX, term_varY]))
fof_exists2      = Fol.Exists (Just "func") (Variable "x") 
                   (Fol.Atm (Fol.Rel "P" [term_varX, term_varY]))
fof_existsPx     = Fol.Exists Nothing (Variable "x") 
                   $ Fol.Atm (Fol.Rel "P" [term_varX])
fof_existsPxa    = Fol.Exists Nothing (Variable "x") 
                   $ Fol.Atm (Fol.Rel "P" [term_varX, term_consA])
fof_PfcxAndQxgyz = Fol.parseFormula "P(f('c, x)) & Q(x, g(y, z))" 
fof_PfcxOrQxgyz  = Fol.parseFormula "P(f('c, x)) | Q(x, g(y, z))" 
fof_PfcxImpQxgyz = Fol.parseFormula "P(f('c, x)) & Q(x, g(y, z))" 
fof_PfcxIffQxgyz = Fol.parseFormula "P(f('c, x)) & Q(x, g(y, z))" 
fof_existsQxfyz  = Fol.parseFormula "exists x. Q(x, f(y, z))" 
