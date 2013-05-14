module Test.TestHelper where

import Data.Set as Set

import Chase.Problem.IModel
import Chase.Problem.Structures

{-| Checks whether two models are identical -}
equalModels :: Model -> Model -> Bool
equalModels m1@(Model trs1 dom1) m2@(Model trs2 dom2) =
    (equalLists trs1 trs2) && (equalLists dom1 dom2)

{-| Returns true if the input frames are identical.
 -}
equalFrames :: Frame -> Frame -> Bool
equalFrames f1@(Frame id1 b1 h1 vs1) f2@(Frame id2 b2 h2 vs2) =
    and [id1 == id2, b1 == b2, h1 == h2, equalLists vs1 vs2]

{-| Returns true if the input problems are identical -}
equalProblems :: Problem -> Problem -> Bool
equalProblems p1@(Problem fs1 m1 q1 sm1 id1 c1) 
              p2@(Problem fs2 m2 q2 sm2 id2 c2) = 
    and [all (\(f1, f2) -> equalFrames f1 f2) $ zip fs1 fs2, 
         equalModels m1 m2, sm1 == sm2, id1 == id2, c1 == c2]

{-| Set comparison for two tests. -}
equalLists :: Ord a => [a] -> [a] -> Bool
equalLists l1 l2 = s1 == s2
    where s1 = Set.fromList l1
          s2 = Set.fromList l2