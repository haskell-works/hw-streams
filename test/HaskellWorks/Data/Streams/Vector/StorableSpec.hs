{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Streams.Vector.StorableSpec
  ( spec
  ) where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Vector.Storable                      as DVS
import qualified HaskellWorks.Data.Streams.Vector.Storable as SDVS
import qualified Hedgehog.Gen                              as G
import qualified Hedgehog.Range                            as R
import qualified Test.Gen                                  as G

{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "HaskellWorks.Data.Stream.Vector.StorableSpec" $ do
  it "map" $ requireProperty $ do
    v <- forAll $ G.vector (R.linear 0 100) (G.word64 (R.constantBounded))

    DVS.map (+ 1) v === SDVS.map (+ 1) v
  it "foldl" $ requireProperty $ do
    v <- forAll $ G.vector (R.linear 0 100) (G.word64 (R.constantBounded))

    DVS.foldl (+) 0 v === SDVS.foldl (+) 0 v
  it "sum" $ requireProperty $ do
    v <- forAll $ G.vector (R.linear 0 100) (G.word64 (R.constantBounded))

    DVS.sum v === SDVS.sum v
