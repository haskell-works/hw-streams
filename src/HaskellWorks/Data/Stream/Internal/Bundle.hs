module HaskellWorks.Data.Stream.Internal.Bundle where

import HaskellWorks.Data.Stream.Chunk (Chunk)

import qualified HaskellWorks.Data.Stream.Stream as S

type Size = Int

data Bundle a = Bundle
  { bundleSize   :: Size
  , bundleElems  :: S.Stream a
  , bundleChunks :: S.Stream (Chunk a)
  }
