module HaskellWorks.Data.Stream.Internal where

inplace :: a -> a
inplace = id
{-# INLINE [1] inplace #-}
