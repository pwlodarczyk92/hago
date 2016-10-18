{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Go.Core.Position(
Player(..), opposite,
rawPlace, rawLiberties,
Position(..), empty, fromStones, fromRawStones,
getStones, getPlayer, liberties, place, placeOnly
) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)

import qualified Go.Core.Stones as Stones
import qualified Go.Core.Fields as Fields
import Go.Core.Fields (Field)
import Go.Core.Stones (Stones, Root)

import Data.List (foldl')
import Data.Tuple (swap)
import Control.Exception (assert)
import Control.Monad (join)


data Player = White | Black
    deriving Eq

instance Eq (Position f) where
    (==) (Position w1 b1) (Position w2 b2) = w1 == w2 && b1 == b2

opposite :: Player -> Player
opposite player = if player == White then Black else White


data Position f where
    Position :: (Field f) => {
        getWhites :: Stones f,
        getBlacks :: Stones f
    } -> Position f

getPlayer :: Position f -> f -> Maybe Player
getPlayer (Position whites blacks) field = if Stones.contains whites field
    then Just White
    else if Stones.contains blacks field
        then Just Black
        else Nothing

getStones :: Position f -> Player -> Stones f
getStones (Position whites blacks) player = case player of
    White -> whites
    Black -> blacks

empty :: (Field f) => Position f
empty = Position Stones.empty Stones.empty

rawLiberties :: (Field f) => Stones f -> Stones f -> Root f -> Set f
rawLiberties player enemy groupRoot = Set.difference (Stones.neighbors player groupRoot) (Stones.allStones enemy)

rawPlace :: forall f . (Field f) => Stones f -> Stones f -> f -> Maybe (Stones f, Stones f, Set f)
rawPlace player enemy field = assert (not $ Stones.contains enemy field) result where

    result = if Set.size killedStones == 0 && Set.size newGroupLiberties == 0
        then Nothing --move forbidden
        else Just (newPlayer, newEnemy, killedStones)

    removeOpponents :: (Stones f, Set f) -> f -> (Stones f, Set f)
    removeOpponents (currentEnemyStones, currentKilledStones) maybeEnemy =
        case Stones.root currentEnemyStones maybeEnemy of
            Nothing        -> (currentEnemyStones, currentKilledStones)
            Just enemyRoot -> if Set.size enemyLiberties > 1
                then (currentEnemyStones, currentKilledStones)
                else (killedEnemy, Set.union currentKilledStones enemyStones)
                where
                    (enemyStones, _, killedEnemy) = Stones.group enemyRoot currentEnemyStones
                    enemyLiberties = rawLiberties currentEnemyStones player enemyRoot


    (newEnemy, killedStones) = foldl' removeOpponents (enemy, Set.empty) (Fields.adjacent field)
    (newPlayer, newRoot) = Stones.put player field
    newGroupLiberties = rawLiberties newPlayer newEnemy newRoot

liberties :: (Field f) => Position f -> Player -> Root f -> Set f
liberties (Position white black) player r = uncurry rawLiberties (swapBy player (white, black)) r

place :: (Field f) => Position f -> Player -> f -> Maybe (Position f, Set f)
place (Position white black) player f = case uncurry rawPlace (swapBy player (white, black)) f of
    Nothing -> Nothing
    Just (current,enemy,result) -> Just (uncurry Position $ swapBy player (current, enemy), result)

placeOnly :: (Field f) => Position f -> Player -> f -> Maybe (Position f)
placeOnly pos p f = fst <$> place pos p f

swapBy :: Player -> (x, x) -> (x, x)
swapBy player pair = case player of
    White -> pair
    Black -> swap pair

--- parsing, serializing

fromStones :: (Field f) => Stones f -> Stones f -> Maybe (Position f)
fromStones whites blacks = if Set.empty == Set.intersection (Stones.allStones whites) (Stones.allStones blacks)
    then Just (Position whites blacks)
    else Nothing

fromRawStones :: (Foldable g, Field f) => g f -> g f -> Maybe (Position f)
fromRawStones whites blacks = join $ fromStones <$> (Stones.fromRawStones whites) <*> (Stones.fromRawStones blacks)