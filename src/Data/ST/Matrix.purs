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

import Control.Monad.Eff
import Control.Monad.ST (ST())

import Data.TypeNat
import Data.Array.ST hiding (freeze, thaw)

import Control.Apply

import qualified Data.Matrix as M


newtype STMat s h a = STMat (STArray h a)

-- try array cloning with .slice() instead of the for-loop
-- implementation in Data.Array.ST. Needs benchmarking.
foreign import copyImpl :: forall a b h r. a -> Eff (st :: ST h | r) b

-- | Create an immutable copy of a mutable array.
freeze :: forall a h r. STArray h a -> Eff (st :: ST h | r) (Array a)
freeze = copyImpl

-- | Create a mutable copy of an immutable array.
thaw :: forall a h r. Array a -> Eff (st :: ST h | r) (STArray h a)
thaw = copyImpl

-- | Freeze an ST array. Do not mutate the STArray afterwards!
foreign import unsafeFreeze :: forall a h. STArray h a -> Array a

foreign import unsafeThaw :: forall a h. Array a -> STArray h a

cloneSTMat :: forall s h a r. (STMat s h a) -> Eff (st :: ST h | r) (STMat s h a)
cloneSTMat (STMat arr) = STMat <<< unsafeThaw <$> freeze arr

fromSTMat :: forall s h a r. (Sized s) => (STMat s h a) -> Eff (st :: ST h | r) (M.Mat s a)
fromSTMat (STMat arr) = do
    x   <- freeze arr
    return (M.fromArray x)

toSTMat :: forall s h a r. (M.Mat s a) -> Eff (st :: ST h | r) (STMat s h a)
toSTMat m = STMat <$> thaw (M.toArray m)

-- copyToSTMat :: forall s h a r. (M.Matrix (M.Mat s) a) => (M.Mat s a) -> (STMat s h a) -> Eff (st :: ST h | r) Unit

foreign import copyToSTMat :: forall s h a r. (M.Mat s a) -> (STMat s h a) -> Eff (st :: ST h | r) Unit

identityST' :: forall s h r. (Sized s) => Eff (st :: ST h | r) (STMat s h Number)
identityST' =
    let m = M.identity' :: M.Mat s Number
    in STMat <$> thaw (M.toArray m)

foreign import scaleSTMatrixInt :: forall a h r. (Num a) => a -> STArray h a -> Eff (st :: ST h | r) Unit

scaleSTMatrix :: forall s a h r. (Num a) => a -> (STMat s h a) -> Eff (st :: ST h | r) (STMat s h a)
scaleSTMatrix x v@(STMat arr) = scaleSTMatrixInt x arr *> return v

fromMatrix :: forall s h r a. M.Mat s a -> Eff (st :: ST h | r) (STMat s h a)
fromMatrix (M.Mat m) = STMat <$> thaw m

foreign import runSTMatrix :: forall s a r. (forall h. Eff (st :: ST h | r) (STMat s h a)) -> Eff r (M.Mat s a)
