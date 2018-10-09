{-# LANGUAGE GADTs #-}

module HaskellWorks.Data.Stream where

data Stream a where
  Stream :: ()
    => (s -> Step s a)
    -> s
    -> Int
    -> Stream a

data Step s a
  = Yield a s
  | Skip s
  | Done
