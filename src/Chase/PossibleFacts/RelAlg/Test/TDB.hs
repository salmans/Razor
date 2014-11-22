{- Razor
   Module      : Chase.PossibleFacts.RelAlg.Test.TDB
   Description : Unit tests for database operations
   Maintainer  : Salman Saghafi -}

module Chase.PossibleFacts.RelAlg.Test.TDB where

-- Standard
import Control.Applicative

-- HUnit
import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Test
import Chase.PossibleFacts.RelAlg.Test.TestData

-- Syntax
import Syntax.Term (Variable (..))

-- RelAlg
import qualified Tools.ExtendedSet as ExSet
import Chase.PossibleFacts.RelAlg.IDB


test_emptySet =
    ["test emptySet" ~: (Set ExSet.empty  :: Set Int) ~=? emptySet]

test_fromList = 
    [ "test fromList for empty list" ~: (emptySet  :: Set Int) ~=? fromList []
    , "test fromList for a non-empty list" ~: 
      (Set (ExSet.fromList [1, 2, 3]) :: Set Int) ~=? fromList [1, 2, 3]]

-- test_functorSet = 
--     [ "test fmap for empty set" ~: 
--      emptySet ~=? fmap (+1) emptySet

--     , "test fmap for non-empty set" ~: 
--      fromList [2, 3, 4] ~=? (fmap (+1) $ fromList [1, 2, 3])
--     ]

-- test_applicativeSet = 
--     [ "test pure" ~:
--      (fromList [1] :: Set Int) ~=? pure 1

--     , "test <*> for empty set" ~: 
--      Set [] ~=? Set [(+1)] <*> emptySet

--     , "test <*> for non-empty set" ~: 
--      Set [2, 3, 4] ~=? Set [(+1)] <*> Set [1, 2, 3]

--     , "test <*> for non-empty set with multiple functions" ~: 
--      Set [2, 3, 4, 3, 4, 5] ~=? Set [(+1), (+2)] <*> Set [1, 2, 3]

--     , "test <$> for empty set" ~: 
--      Set [] ~=? (+1) <$> Set []

--     , "test <$> for non-empty set" ~: 
--      Set [2, 3, 4] ~=? (+1) <$> Set [1, 2, 3]
--     ]

test_project =
    [ "test project for empty set" ~: 
     (emptySet :: Set [Char]) ~=? project (Project show) (emptySet :: Set Int)
    ,  "test project for non-empty set" ~: 
     (fromList ["1", "2"] :: Set [Char]) ~=? 
     project (Project show) (fromList [1,2])
    ]
-- property: project is map

test_select =
    [ "test select for empty set" ~: 
     (emptySet :: Set Int) ~=? select (Select (>1)) (emptySet :: Set Int)
    ,  "test select for non-empty set" ~: 
     (fromList [2, 3] :: Set Int) ~=? select (Select (>1)) (fromList [1, 2, 3])
    ]
-- property: select is filter

test_join =
    [ "test select for empty set on left" ~: 
     emptySet ~=? join (Select (\(x,y) -> x > 1 && y < 3)) 
                  emptySet (fromList [1, 2, 3])
    , "test select for empty set on right" ~: 
     emptySet ~=? join (Select (\(x,y) -> x > 1 && y < 3)) 
                  (fromList [1, 2, 3]) emptySet
    , "test select for non-empty sets" ~: 
     (fromList [(2, 1), (2, 2), (3, 1), (3,2)]) ~=? 
     join (Select (\(x,y) -> x > 1 && y < 3)) 
              (fromList [1, 2, 3]) (fromList [1, 2, 3])
    ]    

test_union = [ "test union for empty sets" ~: 
               emptySet ~=? union emptySet (emptySet :: Set Int)
             , "test union with empty set on left" ~:
               let set = fromList [1,2,3]
               in set ~=? union emptySet set
             , "test union with empty set on right" ~:
               let set = fromList [1,2,3]
               in set ~=? union set emptySet
             , "test union for sets without elements in common" ~:
               (fromList [1,2,3,4,5]) ~=? 
               union (fromList [1,2,3]) (fromList [4,5])
             , "test union for sets with elements in common" ~:
               (fromList [1,2,3,5]) ~=? 
               union (fromList [1,2,3]) (fromList [1,5])
             , "test union for identical sets" ~:
               let set = fromList [1,2,3]
               in set ~=? union set set
             ]

test_difference = 
    [ "test difference for empty sets" ~: 
      emptySet ~=? difference emptySet (emptySet :: Set Int)
    , "test difference with empty set on left" ~:
      emptySet ~=? difference emptySet (fromList [1,2])
    , "test difference with empty set on right" ~:
      let set = fromList [1,2,3]
      in set ~=? difference set emptySet
    , "test difference for sets without elements in common" ~:
      (fromList [1,2,3]) ~=? 
      difference (fromList [1,2,3]) (fromList [4,5])
    , "test difference for sets with elements in common" ~:
      (fromList [2,3]) ~=? 
      difference (fromList [1,2,3]) (fromList [1,5])
    , "test difference for identical sets" ~:
      emptySet ~=? difference (fromList [1,2,3]) (fromList [1,2,3])
    ]

test_all = test_emptySet ++ test_fromList 
           ++ test_project ++ test_select ++ test_join
           ++ test_union ++ test_difference