{-# LANGUAGE GADTs #-}

module HaskellWorks.Data.Stream where

data Stream a where
  Stream :: ()
    => (s -> Step s a)
    -> s
    -> Int
    -> Stream a

instance Functor Stream where
  fmap f (Stream step s i) = Stream (fmap f <$> step) s i
  {-# INLINE fmap #-}

data Step s a
  = Yield a s
  | Skip s
  | Done

instance Functor (Step s) where
  fmap f (Yield a s) = Yield (f a) s
  fmap _ (Skip s)    = Skip s
  fmap _ Done        = Done
  {-# INLINE fmap #-}
