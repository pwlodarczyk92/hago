{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Go.Stones (f19SingletonTest, singletonTest, neighborsTest, f19NeighborsTest) where

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Go.Parse
import qualified Go.Core.Stones as Stones
import qualified Go.Core.Fields as Fields
import Go.Core.Fields (F19, Field, ExtField)
import Go.Core.Stones (Stones)

import Data.Maybe (fromJust, isJust)
import Debug.Trace (trace)

fromSJust :: Maybe x -> x
fromSJust x = if isJust x then fromJust x else error "Tests.Go.Stones"

singletonTest :: forall f . (Field f) => f -> Bool
singletonTest tField = result where
    tStones :: Maybe (Stones f)
    tStones = Stones.fromRawStones [tField]
    result = case tStones of
        Nothing -> False
        Just jStones -> case Stones.root jStones tField of
            Nothing -> False
            Just jRoot ->
                Stones.field jRoot == tField && -- tField is only element of jRoot group, so it has to be its representant
                Stones.roots jStones == [jRoot] && -- jRoot is only group of jStones
                Stones.rootSet jStones == Set.singleton jRoot && -- jRoot is only group of jStones
                Stones.contains jStones tField && -- tField is a stone of jStones
                all (not . Stones.contains jStones) (Set.delete tField Fields.allFields) && -- tField is the only stone of jStones
                Stones.allStones jStones == Stones.stones jStones jRoot && -- jRoot is only group of jStones
                Stones.stones jStones jRoot == Set.singleton tField && -- tField is only element of jRoot group
                Stones.neighbors jStones jRoot == Fields.adjacent tField && -- all tField neighbors are it's gtoup neighbors
                Stones.remove jStones jRoot == Stones.empty && -- obvious
                Stones.group jRoot jStones == (Stones.stones jStones jRoot,
                                               Stones.neighbors jStones jRoot,
                                               Stones.remove jStones jRoot) -- composite call check

f19SingletonTest = all singletonTest (Fields.allFields :: Set F19)

exGroup1 = [(1, 1), (1, 2), (2, 2)]
exGroup2 = [(0, 0), (0, 1), (0, 2), (0, 3)]
exGroup3 = [(2, 2), (3, 3), (2, 3), (2, 4)]
exNeighbors1 = [(0, 1), (0, 2), (1, 3), (2, 3), (3, 2), (2, 1), (1, 0)]
exNeighbors2 = [(1, 0), (1, 1), (1, 2), (1, 3), (0, 4)]
exNeighbors3 = [(1, 2), (1, 3), (1, 4), (2, 5), (3, 4), (4, 3), (3, 2), (2, 1)]
examplePairs = [(exGroup1, exNeighbors1), (exGroup2, exNeighbors2), (exGroup3, exNeighbors3)]

unrawTestCase :: (ExtField f) => ([(Int, Int)], [(Int, Int)]) -> ([f], [f])
unrawTestCase (exGroup, exNeighbors) = (fmap (fromSJust . Fields.encode) exGroup, fmap (fromSJust . Fields.encode) exNeighbors)

neighborsTest :: (Field f) => [f] -> [f] -> Bool
neighborsTest tRawStones tNeighbors = result where
    result = case Stones.fromRawStones (Set.fromList tRawStones) of
        Nothing -> False
        Just jStones ->
            (length (Stones.roots jStones) == 1) &&
            Stones.neighbors jStones (head (Stones.roots jStones)) == Set.fromList tNeighbors

f19NeighborsTest :: [Bool]
f19NeighborsTest = map (uncurry neighborsTest) unrawCases where
    unrawCases :: [([F19], [F19])]
    unrawCases = fmap unrawTestCase examplePairs
