{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Go.Core.Stones(
Stones(..), Root, -- types (root should not be constructed outside module!)
empty, fromRawStones, -- constants/constructors
contains, allStones, rootSet, root, roots,-- stones accessors
stones, neighbors, field, -- group accessors
put, putOnly, -- stones modifiers
remove, -- group modifier
group, -- composite functions
) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)

import qualified Go.Core.Fields as Fields
import Go.Core.Fields (Field)

import Control.Monad (foldM)
import Data.Maybe (fromJust, isJust)
import Control.Exception (assert)

fromSJust :: Maybe x -> x
fromSJust x = if isJust x then fromJust x else error "Go.Core.Stones"

newtype Root f = Root { field :: f }
    deriving(Eq, Ord)

data Stones f where
    Stones :: (Field f, Ord f, Eq f) => {
        rootMap     :: Map f (Root f),
        groupMap    :: Map (Root f) (Set f),
        neighborMap :: Map (Root f) (Set f)
    } -> Stones f

instance Eq f => Eq (Stones f) where
    (==) x y = let k i = Map.keysSet (rootMap i) in k x == k y


empty :: (Field f, Ord f) => Stones f
empty = Stones Map.empty Map.empty Map.empty

put :: forall f . (Field f) => Stones f -> f -> (Stones f, Root f)
put (Stones rMap gMap nMap) aField = assert (Map.notMember aField rMap) result where

    result         = ((Stones newRoots newGroups newNeighbors), newRoot)
    adjacents      = Fields.adjacent aField
    (taken, free)  = Set.partition (flip Map.member rMap) adjacents -- unique roots, no nothing
    mayRootSet     = Set.map (fromSJust . flip Map.lookup rMap) taken
    lonely         = Set.null taken
    newRoot        = Root aField

    mergeNeighbourVals :: Map (Root f) (Set f) -> Set f
    mergeNeighbourVals dict = Set.unions $ fmap (fromSJust . flip Map.lookup dict) (Set.toList mayRootSet)

    newGroup      = if   lonely
                    then Set.singleton aField
                    else Set.insert aField $ mergeNeighbourVals gMap
    newRoots      = Map.union (Map.fromSet (\k -> newRoot) newGroup) rMap
    newGroups     = Map.insert newRoot newGroup     (if lonely then gMap else Map.withoutKeys gMap mayRootSet)
    newNeighbour  = if   lonely
                    then adjacents
                    else Set.union free $ Set.delete aField $ mergeNeighbourVals nMap
    newNeighbors  = Map.insert newRoot newNeighbour (if lonely then nMap else Map.withoutKeys nMap mayRootSet)

remove :: (Field f, Ord f) => Stones f -> Root f -> Stones f
remove (Stones rMap gMap nMap) aRoot = Stones newRoots newGroups newNeighbors where
    removedStones = fromSJust $ Map.lookup aRoot gMap
    newNeighbors  = Map.delete aRoot nMap
    newGroups     = Map.delete aRoot gMap
    newRoots      = Map.withoutKeys rMap removedStones

putOnly :: (Field f) => Stones f -> f -> Stones f
putOnly s f = fst (put s f)

contains :: (Field f) => Stones f -> f -> Bool
contains s f = Map.member f $ rootMap s

allStones :: Stones f -> Set f
allStones (Stones rMap _ _) = Map.keysSet rMap

root :: Stones f -> f -> Maybe (Root f)
root (Stones rMap _ _) f = Map.lookup f rMap

roots :: Stones f -> [Root f]
roots (Stones _ gMap _) = Map.keys gMap

rootSet :: Stones f -> Set (Root f)
rootSet (Stones _ gMap _) = Map.keysSet gMap

stones, neighbors :: (Ord f, Field f) => Stones f -> Root f -> Set f
group   :: (Field f) => Root f -> Stones f -> (Set f, Set f, Stones f)
stones    (Stones _ gMap _) root = fromSJust (Map.lookup root gMap)
neighbors (Stones _ _ nMap) root = fromSJust (Map.lookup root nMap)
group s r = (stones r s, neighbors r s, remove r s)


--- parsing, serializing


fromRawStones :: (Foldable g, Field f) => g f -> Maybe (Stones f)
fromRawStones stones = foldM (\s f -> if contains s f then Nothing else Just (putOnly s f)) empty stones
