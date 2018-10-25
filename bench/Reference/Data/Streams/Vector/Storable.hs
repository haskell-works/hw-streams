module Reference.Data.Streams.Vector.Storable where

import Data.Vector.Storable (Storable)

import qualified Data.Vector.Storable as DVS

dotp :: (Storable a, Num a) => DVS.Vector a -> DVS.Vector a -> a
dotp v w = DVS.sum (DVS.zipWith (*) v w)
