module HaskellWorks.Data.Streams.Internal.Bundle where

import HaskellWorks.Data.Streams.Chunk (Chunk)

import qualified HaskellWorks.Data.Streams.Stream as S

type Size = Int

data Bundle a = Bundle
  { bundleSize   :: Size
  , bundleElems  :: S.Stream a
  , bundleChunks :: S.Stream (Chunk a)
  }
