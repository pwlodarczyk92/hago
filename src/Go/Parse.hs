{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Go.Parse where

import qualified Data.Set as Set
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON), Value(Object, String), (.:), (.=), (.:?), (.!=))
import Data.Aeson.Types (Parser)

import qualified Go.Core.Fields as Fields
import qualified Go.Core.Stones as Stones
import qualified Go.Core.Position as Position
import qualified Go.Core.Situation as Situation
import qualified Go.Core.Tree as Tree
import Go.Core.Fields (Field, F19)
import Go.Core.Stones (Stones)
import Go.Core.Position (Position, Player(Black, White))
import Go.Core.Situation (Situation, Move(Pass, Place))
import Go.Core.Tree(GameData, PastData)

mayParse :: String -> Parser (Maybe a) -> Parser a
mayParse failmsg mayparser = mayparser >>= result where
    result Nothing = fail failmsg
    result (Just x) = return x

instance ToJSON F19 where
    toJSON f = let (x, y) = Fields.decode f in Aeson.object ["x" .= x, "y" .= y]

instance FromJSON F19 where
    parseJSON (Object v) = mayParse "coordinates out of bounds" $
                           Fields.encode <$> ((,) <$> v .: "y" <*> v .: "x")
    parseJSON invalid = AesonTypes.typeMismatch "F19" invalid

instance (ToJSON f) => ToJSON (Stones f) where
    toJSON stones = toJSON $ Stones.allStones stones

instance (FromJSON f, Field f) => FromJSON (Stones f) where
    parseJSON v = mayParse "duplicate stones" $
                  Stones.fromRawStones <$> (parseJSON v :: Parser [f])

instance ToJSON Player where
    toJSON White = String "WHITE"
    toJSON Black = String "BLACK"

instance FromJSON Player where
    parseJSON "WHITE" = pure White
    parseJSON "BLACK" = pure Black
    parseJSON invalid = AesonTypes.typeMismatch "WHITE or BLACK" invalid

instance (ToJSON f) => ToJSON (Position f) where
    toJSON position = Aeson.object ["whites" .= Position.getWhites position, "blacks" .= Position.getBlacks position]

instance (FromJSON f, Field f) => FromJSON (Position f) where
    parseJSON (Object v) = mayParse "overlapping whites and blacks" $
                           Position.fromStones <$> v .: "whites" <*> v .: "blacks"
    parseJSON invalid = AesonTypes.typeMismatch "Position" invalid


instance (ToJSON f) => ToJSON (Situation f) where
    toJSON situation = Aeson.object
        ["position" .= Situation.position situation,
        "whiteScore" .= Situation.whitePoints situation,
        "blackScore" .= Situation.blackPoints situation,
        "passCount" .= Situation.passCount situation,
        "currentPlayer" .= Situation.currentPlayer situation]

instance (FromJSON f, Field f) => FromJSON (Situation f) where
    parseJSON (Object v) = mayParse "more than 2 consecutive passes" $
                           Situation.fromData <$>
                           v .: "position" <*>
                           v .:? "whiteScore" .!= 0 <*>
                           v .:? "blackScore" .!= 0  <*>
                           v .:? "passCount" .!= 0  <*>
                           v .:? "currentPlayer" .!= Black
    parseJSON invalid = AesonTypes.typeMismatch "Situation" invalid

instance (ToJSON f) => ToJSON (Move f) where
    toJSON Pass = String "null"
    toJSON (Place f) = toJSON f

instance (FromJSON f) => FromJSON (Move f) where
    parseJSON (String "null") = return Pass
    parseJSON x = Place <$> parseJSON x

instance (ToJSON f) => ToJSON (GameData f) where
    toJSON gamedata = result where
        (moves, start) = Tree.toHistory gamedata
        result = Aeson.object ["root" .= start, "moves" .= moves]

instance (FromJSON f, Field f) => FromJSON (GameData f) where
    parseJSON (Object v) = mayParse "invalid data, probably impossible move sequence" $
                           Tree.fromHistory <$>
                           v .: "root" <*>
                           v .: "moves"
    parseJSON invalid = AesonTypes.typeMismatch "Situation" invalid

