module Baseline.Data.Stream.Vector.Storable where

import qualified Data.Vector.Storable                     as DVS
import qualified HaskellWorks.Data.Stream.Vector.Storable as DVS

-- | Version of map that does not do fusion
map :: (DVS.Storable a, DVS.Storable b) => (a -> b) -> DVS.Vector a -> DVS.Vector b
map f = DVS.unstream . fmap f . DVS.stream
{-# INLINE map #-}
