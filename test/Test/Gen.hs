module Test.Gen where

import Data.Vector.Storable (Storable)
import Hedgehog

import qualified Data.Vector.Storable as DVS
import qualified Hedgehog.Gen         as G

vector :: (MonadGen m, Storable a) => Range Int -> m a -> m (DVS.Vector a)
vector r g = DVS.fromList <$> G.list r g
