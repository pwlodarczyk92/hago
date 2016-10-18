{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Go.Immortal (ImmortalCase, test, testf19) where

import qualified Data.Set as Set
import qualified Data.Aeson.Types as AesonTypes
import Data.Set (Set)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))

import qualified Go.Parse -- instances required for parsing
import qualified Go.Core.Stones as Stones
import qualified Go.Algorithms.Immortal as Immortal
import Go.Core.Fields (Field, F19)
import Go.Core.Stones (Stones)

data ImmortalCase f where
    ImmortalCase :: (Field f) => {
        testStones :: Stones f,
        immortals :: Set.Set f
    } -> ImmortalCase f

instance (FromJSON f, Field f) => FromJSON (ImmortalCase f) where
    parseJSON (Object o) = ImmortalCase <$> o .: "points" <*> o .: "immortals"
    parseJSON invalid = AesonTypes.typeMismatch "{\"points\" : [stone], \"immortals\" : [stone]}" invalid

test :: (Field f) => ImmortalCase f -> Bool
test iCase = (==) expected $ Set.unions (Stones.stones tStones <$> result) where
    tStones = testStones iCase
    expected = immortals iCase
    result = Set.toList $ Immortal.groups (Immortal.getImmortals tStones)

testf19 :: ImmortalCase F19 -> Bool
testf19 = test
