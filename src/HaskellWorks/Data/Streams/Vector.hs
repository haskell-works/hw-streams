{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Streams.Vector where

import Control.Monad.ST
import HaskellWorks.Data.Streams.Internal
import HaskellWorks.Data.Streams.Stream

import qualified Data.Vector                      as DV
import qualified Data.Vector.Mutable              as DVM
import qualified HaskellWorks.Data.Streams.Stream as HW

unstream :: forall a. HW.Stream a -> DV.Vector a
unstream (HW.Stream step initialState n) = runST $ do
  v <- DVM.unsafeNew n
  loop step v 0 initialState
  where loop :: (s -> Step s a) -> DVM.MVector t a -> Int -> s -> ST t (DV.Vector a)
        loop g v i s = case g s of
          Yield a s' -> do
            DVM.unsafeWrite v i a
            loop g v (i + 1) s'
          Skip s0 -> loop g v i s0
          Done -> DV.freeze v
{-# INLINE [1] unstream #-}

stream :: forall a. DV.Vector a -> Stream a
stream v = Stream step 0 len
  where len = DV.length v
        step i = if i >= len
          then Done
          else Yield (DV.unsafeIndex v i) (i + 1)
{-# INLINE [1] stream #-}

map :: (a -> b) -> DV.Vector a -> DV.Vector b
map f = unstream . inplace (fmap f) . stream
{-# INLINE map #-}

{-# RULES
  "stream/unstream" forall f. stream (unstream f) = f
  #-}
