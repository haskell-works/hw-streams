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

zipWiths :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWiths f (Stream stepa sa na) (Stream stepb sb nb) = Stream step (sa, sb, Nothing) (min na nb)
  where step (ta, tb, Nothing) = case stepa ta of
          Yield xa ta0 -> Skip (ta0, tb, Just xa)
          Skip ta0     -> Skip (ta0, tb, Nothing)
          Done         -> Done
        step (ta, tb, Just xa) = case stepb tb of
          Yield y tb0 -> Yield (f xa y) (ta, tb0, Nothing)
          Skip tb0    -> Skip (ta, tb0, Just xa)
          Done        -> Done
