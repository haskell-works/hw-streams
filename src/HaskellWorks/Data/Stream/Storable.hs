{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Stream.Storable where

import Control.Monad.ST
import HaskellWorks.Data.Stream

import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM
import qualified HaskellWorks.Data.Stream     as HW

unstream :: forall a. DVS.Storable a => HW.Stream a -> DVS.Vector a
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
