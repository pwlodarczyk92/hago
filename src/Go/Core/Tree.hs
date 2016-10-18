{-# LANGUAGE GADTs #-}
module Go.Core.Tree(GameData, PastData,
situation, pastData, lastMove, lastResult, lastState,
move, makeMove,
makeRoot, empty,
toHistory, fromHistory, situationsList) where

import Data.Map (Map)
import Data.Set (Set)

import qualified Go.Core.Situation as Situation
import Go.Core.Fields (Field)
import Go.Core.Stones (Root)
import Go.Core.Position (Player)
import Go.Core.Situation (Situation, Move)

import Control.Monad (foldM)

data GameData f where
    GameData :: {
        _situation :: Situation f,
        _pastData :: Maybe (PastData f)
    } -> GameData f
data PastData f where
    PastData :: {
        _lastMove :: Move f,
        _lastResult :: Set f,
        _lastState :: GameData f
    } -> PastData f
data Node f where
    Node :: {
        _gameData :: GameData f,
        _moveOptions :: Map (Move f) (Maybe (Node f))
    } -> Node f

lastMove :: PastData f -> Move f
lastMove (PastData lm _ _) = lm
lastResult :: PastData f -> Set f
lastResult (PastData _ lr _) = lr
lastState :: PastData f -> GameData f
lastState (PastData _ _ ls) = ls
pastData :: GameData f -> Maybe (PastData f)
pastData (GameData _ pd) = pd
situation :: GameData f -> Situation f
situation (GameData aSituation _) = aSituation

situationsList :: GameData f -> [Situation f]
situationsList (GameData s mpd) = s : case mpd of
    Nothing -> []
    Just pd -> situationsList $ _lastState pd

toHistory :: GameData f -> ([Move f], Situation f)
toHistory x = toHistory' x []
toHistory' (GameData situation Nothing) list = (list, situation)
toHistory' (GameData _ (Just (PastData lmove _ ldata))) list = toHistory' ldata (lmove : list)

fromHistory :: (Field f) => Situation f -> [Move f]-> Maybe (GameData f)
fromHistory start = foldM move (makeRoot start)

makeRoot :: Situation f -> GameData f
makeRoot situation = GameData situation Nothing

empty :: (Field f) => GameData f
empty = GameData Situation.empty Nothing

violatesKo :: Situation f -> Situation f -> Bool
violatesKo newSituation oldSituation =
    (Situation.passCount oldSituation == 0) &&
    (Situation.position oldSituation == Situation.position newSituation)

violatesAny :: GameData f -> Situation f -> Bool
violatesAny gameData newSituation =
    (Situation.passCount newSituation == 0) &&
    any (violatesKo newSituation) (situationsList gameData)

move :: (Field f) => GameData f -> Move f -> Maybe (GameData f)
move gameData newMove = do
    (newSituation, killed) <- Situation.move (situation gameData) newMove
    if violatesAny gameData newSituation
        then Nothing
        else Just $ GameData newSituation (Just $ PastData newMove killed gameData)

makeMove :: (Field f) => GameData f -> f -> Maybe (GameData f)
makeMove gameData newMove = move gameData (pure newMove)
