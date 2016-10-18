{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Go.Algorithms.NaiveAtari (solution) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)

import qualified Go.Core.Fields as Fields
import qualified Go.Core.Stones as Stones
import qualified Go.Core.Position as Position
import qualified Go.Core.Situation as Situation
import qualified Go.Core.Tree as Tree
import Go.Core.Fields (Field)
import Go.Core.Stones (Root)
import Go.Core.Position (Player)
import Go.Core.Situation (Situation, Move)
import Go.Core.Tree (GameData)

import Debug.Trace (trace)
import Data.Maybe (catMaybes, fromJust, isJust)
import Control.Exception (assert)

fromSJust :: Maybe x -> x
fromSJust x = if isJust x then fromJust x else error "Go.Algorithms.NaiveAtari"


solution :: (Show f, Field f) => f -> GameData f -> Bool
solution field game = rawSolution (fromSJust $ Position.getPlayer position field) field game where
    position = Situation.position . Tree.situation $ game

rawSolution :: (Show f, Field f) => Player -> f -> GameData f -> Bool
rawSolution player field game = result where
    situation = Tree.situation game
    tags = isDead player field situation
    result = case tags of
        Dead -> True
        Alive -> False
        Options opts -> outcome where
            currentPlayer = Situation.currentPlayer situation
            nextSituations = catMaybes $ fmap (Tree.move game . pure) (Set.toList opts)
            outcome = if player == currentPlayer
                then all (rawSolution player field) nextSituations
                else any (rawSolution player field) nextSituations


data Score f = Dead | Alive | Options (Set f)
isDead :: (Show f, Field f) => Player -> f -> Situation f -> Score f
isDead player field situation = result where
    position = Situation.position situation
    playerStones = Position.getStones position player
    currentPlayer = Situation.currentPlayer situation
    result = case Stones.root playerStones field of
        Nothing -> Dead
        Just root -> case (Set.size playerLiberties, currentPlayer == player) of
                (1,     _) -> Options options
                (2, False) -> Options options
                (_,     _) -> Alive
            where
                playerLiberties = Situation.liberties situation player root
                options = if player /= currentPlayer
                    then playerLiberties
                    else Set.union playerLiberties attackOptions
                enemyStones   = Position.getStones position (Position.opposite player)
                neighbors     = Stones.neighbors playerStones root
                enemyRoots    = catMaybes . Set.toList . Set.map (Stones.root enemyStones) $ neighbors
                enemyLibsSets = fmap (Position.rawLiberties enemyStones playerStones) enemyRoots
                attackOptions = Set.unions $ filter ((==) 1 . Set.size) enemyLibsSets



{-options :: (Field f) => Situation f -> Root f -> Player -> Set f
options situation root player = result where
    pos = Situation.position situation
    libs = Position.liberties pos player root

    result = if player /= Situation.currentPlayer situation
        then libs
        else Set.union libs attackopts

    playerStones  = Position.getStones pos player
    enemyStones   = Position.getStones pos (Position.opposite player)
    neighbors     = Stones.neighbors playerStones root
    enemyRoots    = (catMaybes . Set.toList) $ Set.map (Stones.root enemyStones) neighbors
    enemyLibsSets = fmap (Position.rawLiberties enemyStones playerStones) enemyRoots
    attackopts    = Set.unions $ filter ((==) 1 . Set.size) enemyLibsSets
-}