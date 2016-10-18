{-# LANGUAGE ScopedTypeVariables #-}
module Tests.Go.Fields (fieldInvariants, f19FieldInvariants) where

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Go.Core.Fields as Fields
import Go.Core.Fields (F19, Field, ExtField)

import Control.Exception (assert)
import Data.List (foldl')

fieldInvariants :: forall ftype . (Field ftype, ExtField ftype) => ftype -> Bool
fieldInvariants field = result where

    maxval = Fields.bound :: ftype
    (maxx, maxy) = (Fields.decode :: ftype -> (Int, Int)) Fields.bound
    xtest, ytest :: Int -> Bool
    xtest x = 0 <= x && x <= maxx
    ytest y = 0 <= y && y <= maxy

    boundOk, adjnumOk :: ftype -> Bool
    boundOk f = (xtest . fst . Fields.decode) f && (ytest . snd . Fields.decode) f
    adjnum x y = let border v maxv = (if v == 0 || v == maxv then 0 else 1) in
                2 + border x maxx + border y maxy
    adjnumOk f = uncurry adjnum (Fields.decode f) == Set.size (Fields.adjacent f)

    getall :: Set ftype -> Set ftype -> Set ftype
    getall front current
        | Set.null front = current
        | otherwise = getall newfront (Set.union newfront current) where
            newfront = Set.difference (Set.unions $ Fields.adjacent <$> Set.toList current) current

    result = getall (Set.singleton field) (Set.singleton field) == Fields.allFields && -- all fields are reachable
             all boundOk Fields.allFields && -- all fields are in bounds
             all adjnumOk Fields.allFields -- all fields have proper num of neighbors


f19FieldInvariants :: Bool
f19FieldInvariants =
    fieldInvariants (Fields.bound :: F19) &&
    all fieldInvariants (Fields.allFields :: Set F19)