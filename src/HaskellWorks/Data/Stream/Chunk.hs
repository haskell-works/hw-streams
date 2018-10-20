{-# LANGUAGE RankNTypes #-}

module HaskellWorks.Data.Stream.Chunk where

import Control.Monad.Primitive

import qualified Data.Vector.Storable as DVS

data Chunk a = Chunk
  { chunkSize  :: Int
  , chunkWrite :: forall m. PrimMonad m => DVS.MVector (PrimState m) a -> m ()
  }
