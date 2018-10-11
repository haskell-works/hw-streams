{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad  (join)
import Criterion.Main
import Data.Word

import qualified Baseline.Data.Stream.Vector.Storable     as USDVS
import qualified Data.Vector.Storable                     as DVS
import qualified HaskellWorks.Data.Stream                 as S
import qualified HaskellWorks.Data.Stream.Vector.Storable as SDVS

mkReplicateVector :: IO (DVS.Vector Word64)
mkReplicateVector = do
  let !v = DVS.replicate 100000 (0 :: Word64)
  return v

mkEnumVector :: IO (DVS.Vector Word64)
mkEnumVector = do
  let !v = SDVS.enumFromStepN 0 1 100000
  return v

sumall :: DVS.Vector Word64 -> Word64
sumall = S.foldl (+) 123 . SDVS.stream . SDVS.map (+1) . SDVS.map (+1) . SDVS.map (+1)

runMap :: DVS.Vector Word64 -> DVS.Vector Word64
runMap = SDVS.map (+123) . SDVS.map (+123) . SDVS.map (+123)

runMapUnfused :: DVS.Vector Word64 -> DVS.Vector Word64
runMapUnfused = USDVS.map (+123) . USDVS.map (+123) . USDVS.map (+123)

benchThings :: [Benchmark]
benchThings = join $
  [ [ env mkReplicateVector $ \v -> bgroup "mkReplicateVector"
      [ bench "DVS.map"         (whnf runMap          v)
      , bench "DVS.mapUnfused"  (whnf runMapUnfused   v)
      ]
    ]
  , [ env mkEnumVector $ \v -> bgroup "mkEnumVector"
      [ bench "DVS.map"         (whnf runMap          v)
      , bench "DVS.mapUnfused"  (whnf runMapUnfused   v)
      , bench "DVS.foldl"       (whnf sumall          v)
      ]
    ]
  ]

main :: IO ()
main = defaultMain benchThings

