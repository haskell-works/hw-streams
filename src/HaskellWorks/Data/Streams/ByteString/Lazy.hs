module HaskellWorks.Data.Streams.ByteString.Lazy
  ( stream
  ) where

import Data.Word
import HaskellWorks.Data.Streams.Stream (Stream (..))

import qualified Data.ByteString.Lazy                 as LBS
import qualified HaskellWorks.Data.Streams.ByteString as BS

stream :: LBS.ByteString -> Stream Word8
stream = BS.streamChunks . LBS.toChunks
