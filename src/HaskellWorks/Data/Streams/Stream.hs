{-# LANGUAGE CPP   #-}
{-# LANGUAGE GADTs #-}

module HaskellWorks.Data.Streams.Stream where

import Data.Bool
import HaskellWorks.Data.Streams.Size

import Prelude hiding (drop, zipWith)

data Stream a where
  Stream :: ()
    => (s -> Step s a)
    -> s
    -> Size
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

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (Stream stepa sa na) (Stream stepb sb nb) = Stream step (sa, sb, Nothing) (smaller na nb)
  where step (ta, tb, Nothing) = case stepa ta of
          Yield xa ta0 -> Skip (ta0, tb, Just xa)
          Skip ta0     -> Skip (ta0, tb, Nothing)
          Done         -> Done
        step (ta, tb, Just xa) = case stepb tb of
          Yield y tb0 -> Yield (f xa y) (ta, tb0, Nothing)
          Skip tb0    -> Skip (ta, tb0, Just xa)
          Done        -> Done
        {-# INLINE [0] step #-}
{-# INLINE [1] zipWith #-}

zipWithState :: (a -> b -> s -> (c, s)) -> s -> Stream a -> Stream b -> Stream c
zipWithState f state (Stream stepa sa na) (Stream stepb sb nb) = Stream step (sa, sb, Nothing, state) (smaller na nb)
  where step (ta, tb, Nothing, oldState) = case stepa ta of
          Yield xa ta0 -> Skip (ta0, tb, Just xa, oldState)
          Skip ta0     -> Skip (ta0, tb, Nothing, oldState)
          Done         -> Done
        step (ta, tb, Just xa, oldState) = case stepb tb of
          Yield y tb0 -> let (newValue, newState) = f xa y state in Yield newValue (ta, tb0, Nothing, newState)
          Skip tb0    -> Skip (ta, tb0, Just xa, oldState)
          Done        -> Done
        {-# INLINE [0] step #-}
{-# INLINE [1] zipWithState #-}

enumFromStepN :: Num a => a -> a -> Int -> Stream a
enumFromStepN x y n = x `seq` y `seq` n `seq` Stream step (x, n) (Exact n)
  where step (w, m) | m > 0     = Yield w (w + y, m - 1)
                    | otherwise = Done
        {-# INLINE [0] step #-}
{-# INLINE [1] enumFromStepN #-}

foldl :: (a -> b -> a) -> a -> Stream b -> a
foldl f z (Stream step s _) = loop z s
  where loop za sa = za `seq` case step sa of
          Yield x sb -> loop (f za x) sb
          Skip sb    -> loop za sb
          Done       -> za
{-# INLINE [1] foldl #-}

drop :: Int -> Stream a -> Stream a
drop n s@(Stream step state size) = if n > 0
  then case step state of
    Yield _ newState -> drop (n - 1)  $ Stream step newState (size - 1)
    Skip newState    -> drop  n       $ Stream step newState size
    Done             -> s
  else s

{-# RULES
  "zipWith xs xs [Vector.Stream]" forall f xs. zipWith f xs xs = fmap (\x -> f x x) xs   #-}

append :: Stream a -> Stream a -> Stream a
append (Stream stepa ta na) (Stream stepb tb nb) = Stream step (Left ta) (na + nb)
  where step (Left sa) = case stepa sa of
          Yield x sa' -> Yield x (Left  sa')
          Skip    sa' -> Skip    (Left  sa')
          Done        -> Skip    (Right tb)
        step (Right sb) = case stepb sb of
          Yield x sb' -> Yield x (Right sb')
          Skip    sb' -> Skip    (Right sb')
          Done        -> Done
        {-# INLINE step #-}
{-# INLINE append #-}

singleton :: a -> Stream a
singleton a = Stream (bool Done (Yield a False)) True 1

repeat :: Int -> a -> Stream a
repeat n a = Stream step n 1
  where step i = if i > 0 then Yield a (i - 1) else Done
