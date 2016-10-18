{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Go.Atari (AtariCase, test, testf19) where

import qualified Data.Aeson.Types as AesonTypes
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))

import qualified Go.Parse -- instances required for parsing
import qualified Go.Algorithms.NaiveAtari as Atari
import qualified Go.Core.Tree as Tree
import Go.Core.Situation (Situation)
import Go.Core.Fields (Field, F19)
import Go.Core.Stones (Stones)



data AtariCase f where
    AtariCase :: (Show f, Field f) => {
        situation :: Situation f,
        point :: f,
        isAtari :: Bool
    } -> AtariCase f

instance (Show f, FromJSON f, Field f) => FromJSON (AtariCase f) where
    parseJSON (Object o) = AtariCase <$> o .: "situation" <*> o .: "point" <*> o .: "isAtari"
    parseJSON invalid = AesonTypes.typeMismatch "{\"situation\":.., \"point\":.., \"isAtari\":..,}" invalid

test :: (Show f, Field f) => AtariCase f -> Bool
test iCase = isAtari iCase == Atari.solution (point iCase) (Tree.makeRoot . situation $ iCase)

testf19 :: AtariCase F19 -> Bool
testf19 = test
