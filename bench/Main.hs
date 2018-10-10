{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main
import Data.Word

import qualified Data.Vector.Storable                     as DVS
import qualified HaskellWorks.Data.Stream.Vector.Storable as SDVS

mkEnv :: IO (DVS.Vector Word64)
mkEnv = do
  let !v = DVS.replicate 1000 (0 :: Word64)
  return v

foo :: DVS.Vector Word64 -> DVS.Vector Word64
foo v = SDVS.map (+123) (SDVS.map (+123) v)

benchThings :: [Benchmark]
benchThings =
  [ env mkEnv $ \v -> bgroup "mkEnv"
    [ bench "xxxx" (whnf foo v)
    ]
  ]

main :: IO ()
main = defaultMain benchThings

