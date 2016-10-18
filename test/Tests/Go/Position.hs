{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Go.Position (f19KillTest) where

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Go.Core.Fields as Fields
import qualified Go.Core.Stones as Stones
import qualified Go.Core.Position as Position
import qualified Go.Core.Situation as Situation
import Go.Core.Fields (F19, Field, ExtField)
import Go.Core.Stones (Stones)
import Go.Core.Position (Position, Player(..))


import Data.Maybe (fromJust, isJust)
import Control.Monad(foldM)

fromSJust :: Maybe x -> x
fromSJust x = if isJust x then fromJust x else error "Tests.Go.Position"

exGroup1 = [(1, 1), (1, 2), (2, 2)]
exGroup2 = [(0, 0), (0, 1), (0, 2), (0, 3)]
exGroup3 = [(2, 2), (3, 3), (2, 3), (2, 4)]
exNeighbors1 = [(0, 1), (0, 2), (1, 3), (2, 3), (3, 2), (2, 1), (1, 0)]
exNeighbors2 = [(1, 0), (1, 1), (1, 2), (1, 3), (0, 4)]
exNeighbors3 = [(1, 2), (1, 3), (1, 4), (2, 5), (3, 4), (4, 3), (3, 2), (2, 1)]
examplePairs = [(exGroup1, exNeighbors1), (exGroup2, exNeighbors2), (exGroup3, exNeighbors3)]

unrawTestCase :: (ExtField f) => ([(Int, Int)], [(Int, Int)]) -> ([f], [f])
unrawTestCase (exGroup, exNeighbors) = (fmap (fromSJust . Fields.encode) exGroup, fmap (fromSJust . Fields.encode) exNeighbors)

killTest :: forall f . (Field f) => [f] -> [f] -> Bool
killTest tRawStones tNeighbors = result where
    placeWhite, placeBlack :: Position f -> f -> Maybe (Position f)
    placeWhite p = Position.placeOnly p White
    placeBlack p = Position.placeOnly p Black
    whiteOnly = fromSJust $ foldM placeWhite Position.empty tRawStones
    killedWhite = fromSJust $ foldM placeBlack whiteOnly tNeighbors
    result = Stones.allStones (Position.getWhites killedWhite) == Set.empty &&
             Stones.allStones (Position.getBlacks killedWhite) == Set.fromList tNeighbors

f19KillTest :: [Bool]
f19KillTest = fmap (uncurry killTest) unrawCases where
    unrawCases :: [([F19], [F19])]
    unrawCases = fmap unrawTestCase examplePairs