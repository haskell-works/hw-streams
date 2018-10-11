{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Stream.Vector.Storable
  ( stream
  , unstream

  , map
  , zipWith

  , enumFromStepN
  , foldl
  ) where

import Control.Monad.ST
import Data.Vector.Storable              (Storable)
import HaskellWorks.Data.Stream          (Step (..), Stream (..))
import HaskellWorks.Data.Stream.Internal (inplace)
import Prelude                           hiding (foldl, map, zipWith)

import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM
import qualified HaskellWorks.Data.Stream     as S
import qualified HaskellWorks.Data.Stream     as HW

unstream :: forall a. Storable a => HW.Stream a -> DVS.Vector a
unstream (HW.Stream step initialState n) = runST $ do
  v <- DVSM.unsafeNew n
  loop step v 0 initialState
  where loop :: (s -> Step s a) -> DVSM.MVector t a -> Int -> s -> ST t (DVS.Vector a)
        loop g v i s = case g s of
          Yield a s' -> do
            DVSM.unsafeWrite v i a
            loop g v (i + 1) s'
          Skip s0 -> loop g v i s0
          Done -> DVS.freeze v
{-# INLINE [1] unstream #-}

stream :: forall a. Storable a => DVS.Vector a -> Stream a
stream v = Stream step 0 len
  where len = DVS.length v
        step i = if i >= len
          then Done
          else Yield (DVS.unsafeIndex v i) (i + 1)
{-# INLINE [1] stream #-}

map :: (Storable a, Storable b)
  => (a -> b)
  -> DVS.Vector a
  -> DVS.Vector b
map f = unstream . inplace (fmap f) . stream
{-# INLINE map #-}

zipWith :: (Storable a, Storable b, Storable c)
  => (a -> b -> c)
  -> DVS.Vector a
  -> DVS.Vector b
  -> DVS.Vector c
zipWith f v w = unstream (S.zipWith f (stream v) (stream w))

enumFromStepN :: (Num a, Storable a) => a -> a -> Int -> DVS.Vector a
enumFromStepN x y = unstream . inplace (S.enumFromStepN x y)
{-# INLINE [1] enumFromStepN #-}

foldl :: Storable b => (a -> b -> a) -> a -> DVS.Vector b -> a
foldl f z = inplace (S.foldl f z) . stream
{-# INLINE [1] foldl #-}

{-# RULES
  "stream/unstream" forall f. stream (unstream f) = f
  #-}
