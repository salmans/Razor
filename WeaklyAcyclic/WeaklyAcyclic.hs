{- Time-stamp: <2012-12-01 12:05:34 Salman Saghafi>

   An implementation for the chase algorithm to find models for geometric theories.

   Input Geometric Theories:
   + Existential quantifiers and disjunctions on right
   + Conjunctions on left
   + Equality
   
   Output:
   + True if the theory is weakly-acyclic; False, otherwise.
-}
module WeaklyAcyclic.WeaklyAcyclic where

import Control.Applicative

import Data.List
import qualified  Data.Map
import Data.Maybe
import Debug.Trace (trace)
import Control.Exception -- for assert
import Data.Graph

-- Logic modules:
import Formula.SyntaxGeo
import Utils.GeoUtilities (Sub, fv)
import Tools.Equality
import Data.Tree

-- Data Types:
data Relation = Rel Sym Int deriving (Show)
instance Eq Relation where
    (Rel s1 a1) == (Rel s2 a2) = 
        if s1 == s2
        then if a1 /= a2 
             then error $ "Relation symbol with differnt arities: "
                  ++ s1
             else True
        else False

-- Represents a position in a relation
-- for example: P(x,y) has two positions: Pos "P" 1 and Pos "P" 2
data Position = Pos Sym Int deriving (Show)
instance Eq Position where
    (Pos s1 a1) == (Pos s2 a2) = (s1 == s2) && (a1 == a2)

instance Ord Position where
    (Pos s1 a1) `compare` (Pos s2 a2) = 
        if s1 == s2 
        then a1 `compare` a2
        else s1 `compare` s2

-- Maps positions to a unique ID for every position.
type PositionMap = Data.Map.Map Position Int

-- Represents a dependency graph (for cycle tests)
data DependencyGraph = DepGraph {
      graph :: Graph,
      vertexInfo :: Vertex -> (Position, Int, [Int]),
      keyToVertex :: Int -> Maybe Vertex, 
      specialEdge :: (Vertex, Vertex) -> Bool}

-- A path on the graph (also represents a cycle)
type Path = [Vertex]

-- Returns true if a theory is weakly acyclic; otherwise, return false.
weaklyAcyclic :: [Sequent] -> Bool
weaklyAcyclic sequents = 
    not $ any hasSpecialEdge allCycles
    where depgraph@(DepGraph graph vertInfo _ specEdge) = 
              dependencyGraph sequents
          allCycles = graphCycles graph
          hasSpecialEdge [] = False
          hasSpecialEdge [v2] = False
          hasSpecialEdge (v1:v2:vs) = 
              specEdge (v1, v2) || hasSpecialEdge (v2:vs)


-- Creates a dependency graph for a given theory                            
dependencyGraph :: [Sequent] -> DependencyGraph
dependencyGraph sequents = 
    DepGraph graph vertInfo keyToVert specEdge
    where (graph, vertInfo, keyToVert) = 
              graphFromEdges allEdges
          allEdges = unifyEdgeConstructs posMap $ regulars ++ specials
          posMap = theoryPositions sequents
          regulars = theoryRegularEdges posMap sequents
          specials = theorySpecialEdges posMap sequents
          specEdge (vert1, vert2) = let (_,k1,_) = vertInfo vert1
                                        (_,k2,_) = vertInfo vert2 in
                                     specEdge' (k1, k2)
          specEdge' (key1, key2) = 
              any (\(_, _, keys) -> key2 `elem` keys) 
                  $ filter (\(_, k, _) -> k == key1) specials 


-- Returns a PositionMap from the positions in a theory.
theoryPositions :: [Sequent] -> PositionMap
theoryPositions sequents = buildPositions 1 allRelations
    where allRelations = nub $ concatMap sequentRelations sequents

buildPositions :: Int -> [Relation] -> PositionMap
buildPositions _ [] = Data.Map.empty
buildPositions start (Rel sym arity:rs) = 
    Data.Map.union thisRelationMap (buildPositions last rs)
    where thisRelationMap = Data.Map.fromList $ zip 
                            (map (\x -> (Pos sym x)) [1..arity])
                            [start..]
          last = start + arity

sequentRelations :: Sequent -> [Relation]
sequentRelations (Sequent b h) = 
    union (formulaRelations b) (formulaRelations h)

-- Returns a list of relations in a formula and their arities.
formulaRelations :: Formula -> [Relation]
formulaRelations Tru = []
formulaRelations Fls = []
formulaRelations (Exists _ p) = formulaRelations p
formulaRelations (And p q) = 
    union (formulaRelations p) (formulaRelations q)
formulaRelations (Or p q) = 
    union (formulaRelations p) (formulaRelations q)
formulaRelations (Atm (R sym terms)) = [Rel sym (length terms)]

-- Unifies a set of edge constructs that get converted to regular
-- or special edges of a dependency tree.
unifyEdgeConstructs :: PositionMap -> [(Position, Int, [Int])]
                       -> [(Position, Int, [Int])]
unifyEdgeConstructs posMap edges =
    map (\(pos, key) -> compress pos key (filteredEdges key)) emptyEdges
    where emptyEdges = Data.Map.toList posMap
          filteredEdges key = filter (\(_,y,_) -> y == key) edges
          compress pos key = 
              foldr (\(_,_,keys1) (_,_,keys2) -> 
                         (pos, key, union keys1 keys2)) (pos,key,[]) 

-- Builds the set of regular edges of a dependency graph for
-- a given theory
theoryRegularEdges :: PositionMap -> [Sequent] -> 
                             [(Position, Int, [Int])]
theoryRegularEdges posMap sequents =
    unifyEdgeConstructs posMap allEdges
    where allEdges = concatMap (sequentRegularEdges posMap) sequents

-- Returns the regular edges of a dependency tree for a formula.
sequentRegularEdges :: PositionMap -> Sequent ->
                             [(Position, Int, [Int])]
sequentRegularEdges posMap (Sequent b h) = 
    concatMap varTuples $ Data.Map.keys right
    where left = univVarPositions b
          right = univVarPositions h
          varTuples x = 
              case Data.Map.lookup x left of
                Nothing -> []
                Just l -> 
                    case Data.Map.lookup x right of
                      Nothing -> []
                      Just r -> varRegularEdges posMap x l r


varRegularEdges :: PositionMap -> Sym -> [Position] -> [Position] -> 
                   [(Position, Int, [Int])]
varRegularEdges posMap var left right =
    map (\x -> (x, getIndex x, map getIndex right)) left
    where getIndex x = fromJust $ Data.Map.lookup x posMap

-- Builds the set of special edges of a dependency graph for
-- a given theory
theorySpecialEdges :: PositionMap -> [Sequent] -> 
                             [(Position, Int, [Int])]
theorySpecialEdges posMap sequent =
    unifyEdgeConstructs posMap allEdges
    where allEdges = concatMap (sequentSpecialEdges posMap) sequent

-- Returns the regular edges of a dependency tree for a formula.
sequentSpecialEdges :: PositionMap -> Sequent -> [(Position, Int, [Int])]
sequentSpecialEdges posMap (Sequent b h) = 
    concatMap varTuples $ Data.Map.keys left -- right
    where exisVars = exisVarPositions h
          left = univVarPositions b
          -- right = univVarPositions b
          -- The edges starting from a free var on left
          varTuples x =
              case Data.Map.lookup x left of
                Nothing -> []
                Just l -> varSpecialEdges posMap l exisVars
                    -- case Data.Map.lookup x right of
                    --         Nothing -> []
                    --         Just _ -> varSpecialEdges posMap l exisVars


-- Computes the special edges that end to a given existential variable
-- on right.
-- Parameters:
--   a PositionMap containing all of the positions in the theory
--   positions of free variables on left (starting points of special 
--     edges)
--   positions of existential variables on right (end points of special
--     edges)
-- Returns:
--   a list of positions, their unique indicies and unique indicies
--     of the end points of special edges
varSpecialEdges :: PositionMap -> [Position] -> [Position] -> 
                   [(Position, Int, [Int])]
varSpecialEdges posMap list exists =
    map (\x -> (x, getIndex x, map getIndex exists)) list
    where getIndex x = fromJust $ Data.Map.lookup x posMap


-- Returns a map of universally quantified variables in a given 
-- formula to the posisitons in which they appear.
univVarPositions :: Formula -> Data.Map.Map Sym [Position]
univVarPositions Tru = Data.Map.empty
univVarPositions Fls = Data.Map.empty
univVarPositions (Exists v p) = Data.Map.delete v (univVarPositions p)
univVarPositions (And p q) = Data.Map.unionWith (++)
                         (univVarPositions p) (univVarPositions q)
univVarPositions (Or p q) = Data.Map.unionWith (++)
                        (univVarPositions p) (univVarPositions q)
univVarPositions (Atm (R sym terms)) = 
    positionInRelation 1 sym terms
    where positionInRelation _ _ [] = Data.Map.empty
          positionInRelation i s ((Var x):ts) = 
              Data.Map.union 
                  (Data.Map.singleton x [Pos s i])
                  (positionInRelation (i + 1) s ts)
          -- TODO: for now, we don't have function symbols
          positionInRelation i s (t:ts) = positionInRelation (i+1) s ts


-- Returns a map of existentially quantified variables in a given 
-- formula to the posisitons in which they appear. Also, we consider
-- free variables inside functions as existential variables because
-- they enforce special edges in the dependency graph.
exisVarPositions :: Formula -> [Position]
exisVarPositions p = exisVarPositionsHelper [] p

exisVarPositionsHelper :: [Sym] -> Formula -> [Position]
exisVarPositionsHelper _ Tru = []
exisVarPositionsHelper _ Fls = []
exisVarPositionsHelper varList (Exists v p) = 
    exisVarPositionsHelper (v:varList) p
exisVarPositionsHelper varList (And p q) =
    union (exisVarPositionsHelper varList p) 
              (exisVarPositionsHelper varList q)
exisVarPositionsHelper varList (Or p q) =
    union (exisVarPositionsHelper varList p) 
              (exisVarPositionsHelper varList q)
exisVarPositionsHelper varList (Atm (R sym terms)) = 
    exisPositionInRelation varList 1 sym terms

-- Returns the positions in a tuple in which existential variables 
-- appear.
-- Note: that we treat free varibles that appear inside a function
-- just like existential variables.
-- Parameters:
--   a list of existential variables
--   an index determining the position (starts from 1)
--   relational symbol of the tuple
--   a list of terms in the tuple
exisPositionInRelation :: Vars -> Int -> Sym -> [Term] -> [Position]
exisPositionInRelation _ _ _ [] = []
exisPositionInRelation varList i s ((Var x):ts) = 
    if x `elem` varList
    then (Pos s i):(exisPositionInRelation varList (i + 1) s ts)
    else exisPositionInRelation varList (i+1) s ts
exisPositionInRelation varList i s ((Fn f ts'):ts) =
    if hasVars -- If the function has any variable as subterms
    then (Pos s i):applyRest -- then, add the current position
    else applyRest
    where hasVars = not $ null $ concatMap fv ts'
          applyRest = exisPositionInRelation varList (i + 1) s ts
------------------------------
-- Cycle Detection
------------------------------
-- Returns the cycles of a given graph.
graphCycles :: Graph -> [Path]
graphCycles g = buildCycles (vertices g)
    where verts = vertices g
          buildCycles [] = []
          buildCycles (v:vs) = 
              let others = buildCycles vs in
              others ++ filter (\c -> not (hasCycle others c)) (vertCycles g v)
          hasCycle cs c = any (sameCycles c) cs



-- Returns true if the two given paths form the same cycle;
-- if not, returns false.
sameCycles :: Path -> Path -> Bool
sameCycles [] [] = True
sameCycles [] _ = False
sameCycles _ [] = False
sameCycles p1 p2 =
    case index of
      Nothing -> False
      Just i -> let (l, r) = (if i == 0 
                             then splitAt ((length p1) - 1) p1
                             else splitAt i p1) in
                r ++ (tail l) ++ [(head r)] == p2
    where index = elemIndex (head p2) p1

-- Returns a list of cycles that a given vertex belongs to.
vertCycles :: Graph -> Vertex -> [Path]
vertCycles g v = map (\p -> p ++ [v]) 
                 $ concatMap (vertCyclesHelper g v) (dfs g [v])

vertCyclesHelper :: Graph -> Vertex -> Tree Vertex -> [Path]
vertCyclesHelper g v (Node v' [])
    | (v',v) `elem` (edges g) = [[v']]
    | otherwise = []
vertCyclesHelper g v (Node v' trees) =
    if (v', v) `elem` (edges g)
    then [[v']] ++ others
    else others
    where others = 
              filter (\p -> not (null p))
                     $ concatMap (\t -> case vertCyclesHelper g v t of
                                          [] -> []
                                          p' -> map (v':) p') trees


-- Run
doWeaklyAcyclic thy = weaklyAcyclic $ map parseSequent thy