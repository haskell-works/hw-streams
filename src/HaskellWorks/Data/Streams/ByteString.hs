{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Streams.ByteString
  ( streamChunks
  , stream
  ) where

import Data.Word
import HaskellWorks.Data.Streams.Size
import HaskellWorks.Data.Streams.Stream (Step (..), Stream (..))
import Prelude                          hiding (foldl, map, sum, zipWith)

import qualified Data.ByteString as BS

streamChunks :: [BS.ByteString] -> Stream Word8
streamChunks ass = Stream step (ass, 0) Unknown
  where step (bss, i) = case bss of
          cs:css -> if i < BS.length cs
            then Yield (BS.index cs i) (bss, i + 1)
            else Skip (css, 0)
          [] -> Done
{-# INLINE [1] streamChunks #-}

stream :: BS.ByteString -> Stream Word8
stream bs = Stream step (bs, 0) Unknown
  where step (cs, i) = if i < BS.length cs
          then Yield (BS.index cs i) (cs, i + 1)
          else Done
{-# INLINE [1] stream #-}
