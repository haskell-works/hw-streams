{-# LANGUAGE MagicHash #-}

module HaskellWorks.Data.Streams.Stream.Ops where

import Data.Word
import GHC.Int
import GHC.Prim
import GHC.Word                         hiding (ltWord)
import HaskellWorks.Data.Bits.BitWise   ((.&.), (.<.), (.>.), (.^.), (.|.))
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.Streams.Stream

import qualified HaskellWorks.Data.Bits.BitWise   as BW
import qualified HaskellWorks.Data.Streams.Stream as HW

ltWord :: Word64 -> Word64 -> Word64
ltWord (W64# a#) (W64# b#) = fromIntegral (I64# (ltWord# a# b#))
{-# INLINE ltWord #-}

comp :: Stream Word64 -> Stream Word64
comp = fmap BW.comp

bitwiseAnd :: Stream Word64 -> Stream Word64 -> Stream Word64
bitwiseAnd = HW.zipWith (.&.)

bitwiseOr :: Stream Word64 -> Stream Word64 -> Stream Word64
bitwiseOr = HW.zipWith (.|.)

bitwiseXor :: Stream Word64 -> Stream Word64 -> Stream Word64
bitwiseXor = HW.zipWith (.^.)

bitwiseShiftDown :: Count -> Stream Word64 -> Stream Word64
bitwiseShiftDown n as = HW.zipWith splice as (HW.drop 1 as `append` HW.singleton 0)
  where splice :: Word64 -> Word64 -> Word64
        splice a b = (a .>. n) .|. (b .<. (64 - n))

add :: Stream Word64 -> Stream Word64 -> Stream Word64
add = HW.zipWithState (\a b c -> let d = a + b in (c + d, d `ltWord` a)) 0
