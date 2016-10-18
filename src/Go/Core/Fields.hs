{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Go.Core.Fields(Field(..), ExtField(..), F19) where

import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Set (Set)
import Data.Vector (Vector)


class (Eq f, Ord f) => Field f where
    adjacent :: f -> Set f
    allFields :: Set f

class (Eq f, Ord f) => ExtField f where
    decode :: f -> (Int, Int)
    encode :: (Int, Int) -> Maybe f
    bound :: f


newtype F19 = F19 { untype :: Int }
    deriving (Eq, Ord)


xsize, ysize :: Int
xsize = 19
ysize = 19

coordsOk :: Int -> Int -> Bool
calcField :: Int -> Int -> F19
makeField :: Int -> Int -> Maybe F19

coordsOk x y = (0 <= x) && (x < xsize) && (0 <= y) && (y < ysize)

calcField x y = if coordsOk x y
    then F19 (x+y*xsize)
    else error "wrong constructed field"

makeField x y = if coordsOk x y
    then Just (calcField x y)
    else Nothing

xvals, yvals :: Vector Int
elems :: Vector F19
selems :: Set F19
xvals = Vector.fromList  [x        |y<-[0..ysize-1], x<-[0..xsize-1]]
yvals = Vector.fromList  [y        |y<-[0..ysize-1], x<-[0..xsize-1]]
elems = Vector.fromList  [calcField x y |y<-[0..ysize-1], x<-[0..xsize-1]]
selems = Set.fromList [calcField x y |y<-[0..ysize-1], x<-[0..xsize-1]]

getx, gety :: F19 -> Int
getpos :: F19 -> [Int]
getx f = xvals Vector.! untype f
gety f = yvals Vector.! untype f
getpos f = [getx f, gety f]

isNeighbor :: F19 -> F19 -> Bool
neighbors :: Vector (Set F19)
computeNeighbors, getNeighbors :: F19 -> Set F19

isNeighbor f1 f2 = (==) 1 $ sum $ map abs $ zipWith (-) (getpos f1) (getpos f2)
computeNeighbors f = Set.filter (isNeighbor f) $ Set.fromList $ Vector.toList elems
neighbors = fmap computeNeighbors elems
getNeighbors f = neighbors Vector.! untype f


instance Field F19 where
    adjacent = getNeighbors
    allFields = selems

instance ExtField F19 where
    decode f = (getx f, gety f)
    encode = uncurry makeField
    bound = calcField (xsize-1) (ysize-1)

instance Show F19 where
    show = show . decode
