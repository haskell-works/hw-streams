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
ltWord (W64# a#) (W64# b#) = fromIntegral (I# (ltWord64# a# b#))
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
bitwiseShiftDown n as = HW.zipWith splice bs (HW.drop 1 bs `append` HW.singleton 0)
  where bs = HW.drop i as `append` HW.repeat i 0
        o = n `mod` 64
        i = fromIntegral (n `div` 64)
        splice :: Word64 -> Word64 -> Word64
        splice a b = (a .>. o) .|. (b .<. (64 - o))

add :: Stream Word64 -> Stream Word64 -> Stream Word64
add = HW.zipWithState (\a b c ->
    let d = a + b + c     in
    let e = d `ltWord` a  in
    (d, e)) 0
