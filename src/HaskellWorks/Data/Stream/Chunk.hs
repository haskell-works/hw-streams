{-# LANGUAGE RankNTypes #-}

module HaskellWorks.Data.Stream.Chunk where

import Control.Monad.ST

import qualified Data.Vector.Storable as DVS

data Chunk a = Chunk
  { chunkSize  :: Int
  , chunkWrite :: forall s. DVS.MVector s a -> ST s ()
  }
