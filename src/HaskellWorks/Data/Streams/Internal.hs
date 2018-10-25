module HaskellWorks.Data.Streams.Internal where

inplace :: a -> a
inplace = id
{-# INLINE [1] inplace #-}
