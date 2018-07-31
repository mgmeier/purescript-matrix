-----------------------------------------------------------------------------
--
-- Module      :  ST.Matrix
-- Copyright   :  Michael Karg
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
-- | Binding to mjs library
--
-----------------------------------------------------------------------------

module Data.ST.Matrix where

import Prelude
import Effect (Effect)
import Control.Monad.ST (ST())
import Data.TypeNat (class Sized)
import Data.Array.ST (STArray)
import Data.Matrix as M


newtype STMat s h a = STMat (STArray h a)

-- try array cloning with .slice() instead of the for-loop
-- implementation in Data.Array.ST. Needs benchmarking.
foreign import copyImpl :: forall a b. a -> Effect b

-- | Create an immutable copy of a mutable array.
freeze :: forall a h. STArray h a -> Effect (Array a)
freeze = copyImpl

-- | Create a mutable copy of an immutable array.
thaw :: forall a h. Array a -> Effect (STArray h a)
thaw = copyImpl

-- | Freeze an ST array. Do not mutate the STArray afterwards!
foreign import unsafeFreeze :: forall a h. STArray h a -> Array a

foreign import unsafeThaw :: forall a h. Array a -> STArray h a

cloneSTMat :: forall s h a. (STMat s h a) -> Effect (STMat s h a)
cloneSTMat (STMat arr) = STMat <<< unsafeThaw <$> freeze arr

fromSTMat :: forall s h a. (Sized s) => (STMat s h a) -> Effect (M.Mat s a)
fromSTMat (STMat arr) = do
    x   <- freeze arr
    pure (M.fromArray x)

toSTMat :: forall s h a. (M.Mat s a) -> Effect (STMat s h a)
toSTMat m = STMat <$> thaw (M.toArray m)

-- copyToSTMat :: forall s h a. (M.Matrix (M.Mat s) a) => (M.Mat s a) -> (STMat s h a) -> Effect Unit

foreign import copyToSTMat :: forall s h a. (M.Mat s a) -> (STMat s h a) -> Effect Unit

identityST' :: forall s h. (Sized s) => Effect (STMat s h Number)
identityST' =
    let m = M.identity' :: M.Mat s Number
    in STMat <$> thaw (M.toArray m)

foreign import scaleSTMatrixInt :: forall a h. (EuclideanRing a) => a -> STArray h a -> Effect Unit

scaleSTMatrix :: forall s a h. (EuclideanRing a) => a -> (STMat s h a) -> Effect (STMat s h a)
scaleSTMatrix x v@(STMat arr) = scaleSTMatrixInt x arr *> pure v

fromMatrix :: forall s h a. M.Mat s a -> Effect (STMat s h a)
fromMatrix (M.Mat m) = STMat <$> thaw m

foreign import runSTMatrix :: forall s a. (forall h. Effect (STMat s h a)) -> Effect (M.Mat s a)
