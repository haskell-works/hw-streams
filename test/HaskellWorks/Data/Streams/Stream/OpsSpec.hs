{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Streams.Stream.OpsSpec
  ( spec
  ) where

import Data.Semigroup              ((<>))
import Data.Word
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Vector.Storable                      as DVS
import qualified HaskellWorks.Data.Streams.Stream.Ops      as S
import qualified HaskellWorks.Data.Streams.Vector.Storable as SDVS

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Stream.Vector.StorableSpec" $ do
  it "map" $ requireTest $ do
    u <- forAll $ pure $ DVS.fromList $ replicate 4 (0xffffffffffffffff :: Word64)
    v <- forAll $ pure $ DVS.fromList $ [0x0000000000000001 :: Word64] <> replicate 3 0

    let result = SDVS.unstream (S.add (SDVS.stream u) (SDVS.stream v))

    result === DVS.replicate 4 (0x0000000000000000 :: Word64)
