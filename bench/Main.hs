{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad  (join)
import Criterion.Main
import Data.Word

import qualified Baseline.Data.Stream.Vector.Storable     as USDVS
import qualified Data.Vector.Storable                     as DVS
import qualified HaskellWorks.Data.Stream.Vector.Storable as SDVS

mkEnv :: IO (DVS.Vector Word64)
mkEnv = do
  let !v = DVS.replicate 100000 (0 :: Word64)
  return v

mkEnv2 :: IO (DVS.Vector Word64)
mkEnv2 = do
  let !v = SDVS.enumFromStepN 0 1 100000
  return v

runMap :: DVS.Vector Word64 -> DVS.Vector Word64
runMap = SDVS.map (+123) . SDVS.map (+123) . SDVS.map (+123)

runMapUnfused :: DVS.Vector Word64 -> DVS.Vector Word64
runMapUnfused = USDVS.map (+123) . USDVS.map (+123) . USDVS.map (+123)

benchThings :: [Benchmark]
benchThings = join $
  [ [ env mkEnv $ \v -> bgroup "mkEnv"
      [ bench "DVS.map"         (whnf runMap        v)
      , bench "DVS.mapUnfused"  (whnf runMapUnfused v)
      ]
    ]
  , [ env mkEnv2 $ \v -> bgroup "mkEnv2"
      [ bench "DVS.map"         (whnf runMap        v)
      , bench "DVS.mapUnfused"  (whnf runMapUnfused v)
      ]
    ]
  ]

main :: IO ()
main = defaultMain benchThings
