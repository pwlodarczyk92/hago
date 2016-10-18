{-# LANGUAGE ScopedTypeVariables #-}
module Go.Algorithms.Immortal (Immortal, getImmortals, groups, eyes) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)

import qualified Go.Core.Fields as Fields
import qualified Go.Core.Stones as Stones
import qualified Go.Algorithms as Algorithms
import Go.Core.Fields (Field)
import Go.Core.Stones (Stones, Root)

import Data.Maybe (fromJust, isJust)

fromSJust :: Maybe x -> x
fromSJust x = if isJust x then fromJust x else error "Go.Algorithms.Immortal"

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

newtype KillGroup f = KillGroup (Set f, Set f)
kStones, kNeighbors :: KillGroup f -> Set f
kStones    (KillGroup f) = fst f
kNeighbors (KillGroup f) = snd f

newtype Immortal f = Immortal (Set (Root f), [Set f])
groups :: Immortal f -> Set (Root f)
groups   (Immortal f) = fst f
eyes   :: Immortal f -> [Set f]
eyes     (Immortal f) = snd f

killGroup :: forall f . (Field f) => Stones f -> Root f -> [KillGroup f]
killGroup stonesObj gRoot = fmap makeKillGroup (Algorithms.groupAll groupNeighbors) where
    (groupStones, groupNeighbors, _) = Stones.group gRoot stonesObj
    makeKillGroup kgStones = KillGroup (kgStones, Set.difference kgSurroundings groupStones)
        where kgSurroundings = Set.difference (Set.unions $ Fields.adjacent <$> Set.toList kgStones) kgStones


getImmortals :: forall f . (Field f) => Stones f -> Immortal f
getImmortals st = Immortal (immortalGroups, finalPoints) where

    (immortalGroups, finalPoints) = checkAll Set.empty (Stones.rootSet st) []

    killGroupsMap :: Map (Root f) [KillGroup f]
    killGroupsMap = Map.fromSet (killGroup st) $ Stones.rootSet st

    checkAll :: Set (Root f) -> Set (Root f) -> [Set f] -> (Set (Root f), [Set f])
    checkAll checked unchecked points
        | Set.null unchecked = (checked, Algorithms.groupAll $ Set.unions points)
        | otherwise = uncurry3 checkAll (checkNext checked (Set.delete anyElem unchecked) points anyElem) where
            anyElem = Set.elemAt 0 unchecked

    checkNext :: Set (Root f) -> Set (Root f) -> [Set f] -> Root f -> (Set (Root f), Set (Root f), [Set f])
    checkNext checked unchecked points current = (newChecked, newUnchecked, newPoints) where
        newChecked = if currentCanBeKilled
            then Set.empty
            else Set.insert current checked
        newUnchecked = if currentCanBeKilled
            then Set.union checked unchecked
            else unchecked
        newPoints = if currentCanBeKilled
            then []
            else fmap kStones deadKillGroups ++ points
        currentCanBeKilled = length deadKillGroups < 2 -- if two sets of neighbor stones cannot be filled at once
                                                       -- there is no way to kill the group
        mayBeRootOfImmortal r = Set.member r checked || Set.member r unchecked
        killLibertyTaken :: f -> Bool
        killLibertyTaken field = (==) (Just True) $ mayBeRootOfImmortal <$> Stones.root st field
        isKillGroupDead :: KillGroup f -> Bool
        isKillGroupDead kGroup = all killLibertyTaken (kNeighbors kGroup)
        deadKillGroups = filter isKillGroupDead (fromSJust $ Map.lookup current killGroupsMap)
