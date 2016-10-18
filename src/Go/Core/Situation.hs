{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Go.Core.Situation (
Situation(passCount, whitePoints, blackPoints, currentPlayer, position),
Move(..), fromPosition, fromRawStones, fromData, empty,
move, passMove, makeMove, liberties) where

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Go.Core.Fields as Fields
import qualified Go.Core.Stones as Stones
import qualified Go.Core.Position as Position
import Go.Core.Fields (Field)
import Go.Core.Stones (Stones, Root)
import Go.Core.Position (Player(Black, White), Position(Position))

import Control.Monad (liftM)
import Data.Tuple (swap)


data Situation f where
    Situation :: (Field f, Ord f, Eq f) => {
        position :: Position f,
        whitePoints :: Int,
        blackPoints :: Int,
        passCount :: Int,
        currentPlayer :: Player
    } -> Situation f

instance Eq (Situation f) where
    (==) (Situation pos1 wp1 bp1 pc1 cp1) (Situation pos2 wp2 bp2 pc2 cp2) =
        pos1 == pos2 && wp1 == wp2 && bp1 == bp2 && pc1 == pc2 && cp1 == cp2

data Move f = Pass | Place f
instance Functor Move where
    fmap f Pass = Pass
    fmap f (Place x) = Place (f x)
instance Applicative Move where
    pure x = Place x
    (<*>) Pass _ = Pass
    (<*>) _ Pass = Pass
    (<*>) (Place f) (Place x) = Place (f x)
instance Monad Move where
    (>>=) Pass f = Pass
    (>>=) (Place x) f = f x

move :: forall f . (Field f) => Situation f -> Move f -> Maybe (Situation f, Set f)
move situation mayField = case mayField of
    Pass -> (,) <$> passMove situation <*> pure Set.empty
    Place field -> makeMove situation field

passMove :: forall f . (Field f) => Situation f -> Maybe (Situation f)
passMove (Situation position currPoints oppPoints passes player)
    | passes >= 2 = Nothing
    | otherwise   = Just $ Situation position oppPoints currPoints (passes+1) (Position.opposite player)

makeMove :: forall f . (Field f) => Situation f -> f -> Maybe (Situation f, Set f)
makeMove (Situation position whitePoints blackPoints _ current) field = case Position.place position current field of
    Nothing -> Nothing
    Just (newPosition, killedStones) -> Just (newSituation, killedStones) where
        newWhitePoints = whitePoints + (if White == current then Set.size killedStones else 0)
        newBlackPoints = blackPoints + (if Black == current then Set.size killedStones else 0)
        newSituation = Situation newPosition newWhitePoints newBlackPoints 0 (Position.opposite current)

liberties :: forall f . (Field f) => Situation f -> Player -> Root f -> Set f
liberties (Situation (Position whites blacks) _ _ _ _) player root = case player of
    White -> Position.rawLiberties whites blacks root
    Black -> Position.rawLiberties blacks whites root

empty :: (Field f) => Situation f
empty = Situation Position.empty 0 0 0 Black

--- parsing, serializing

fromData :: (Field f) => Position f -> Int -> Int -> Int -> Player -> Maybe (Situation f)
fromData pos wp bp pc curr = if pc > 2 then Nothing else Just $ Situation pos wp bp pc curr

fromPosition :: (Field f) => Position f -> Situation f
fromPosition position = Situation position 0 0 0 Black

fromRawStones :: (Foldable g, Field f) => g f -> g f -> Maybe (Situation f)
fromRawStones whites blacks = liftM fromPosition $ Position.fromRawStones whites blacks

--- combined functions

{-data PRoot f = PRoot Player (Root f)

neighbors :: (Field f) => PRoot f -> Situation f -> Set f
neighbors (PRoot player root) (Situation cStones oStones _ _ _ nowPlayer) = result where
    stones = if player == nowPlayer then cStones else oStones
    result = Stones.neighbors root stones

libs :: (Field f) => PRoot f -> Situation f -> Set f
libs (PRoot player root) situation = liberties root player situation



contains, allStones, rootSet, root, roots,-- stones accessors
stones, neighbors, field, -- group accessors
put, putOnly, -- stones modifiers
remove, -- group modifier
group, -- composite functions-}