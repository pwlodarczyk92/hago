module Go.Algorithms where

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Go.Core.Fields as Fields
import Go.Core.Fields (Field)

both, neither :: (a->Bool) -> (a->Bool) -> a -> Bool
both f g x = f x && g x
neither f g x = not (f x || g x)

groupAll :: (Field f) => Set f -> [Set f]
groupAll fields = findgroups fields [] where
    predicate = flip Set.member fields
    findgroups rest part
        | null rest = part
        | otherwise = findgroups (Set.difference rest new) (new:part) where
            new = group predicate $ Set.elemAt 0 rest

group :: (Field f) => (f -> Bool) -> f -> Set f
group predicate field = findgroup (Set.singleton field) (Set.singleton field) where
    findgroup part front
         | null front = part -- trace ("new group: " ++ show (size part))
         | otherwise = findgroup newPart newFront where --  trace (show (size newPart) ++ " " ++ show (size newFront))
             isNewFront = both (flip Set.notMember part) predicate
             maybeNewFront = Set.unions (Fields.adjacent <$> Set.toList front)
             newFront = Set.filter isNewFront maybeNewFront
             newPart = Set.union part newFront
