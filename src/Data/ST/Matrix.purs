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
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST())
import Data.TypeNat (class Sized)
import Data.Array.ST (STArray)
import Data.Matrix as M


newtype STMat r c h a = STMat (STArray h a)

-- try array cloning with .slice() instead of the for-loop
-- implementation in Data.Array.ST. Needs benchmarking.
foreign import copyImpl :: forall a b h e. a -> Eff (st :: ST h | e) b

-- | Create an immutable copy of a mutable array.
freeze :: forall a h e. STArray h a -> Eff (st :: ST h | e) (Array a)
freeze = copyImpl

-- | Create a mutable copy of an immutable array.
thaw :: forall a h e. Array a -> Eff (st :: ST h | e) (STArray h a)
thaw = copyImpl

-- | Freeze an ST array. Do not mutate the STArray afterwards!
foreign import unsafeFreeze :: forall a h. STArray h a -> Array a

foreign import unsafeThaw :: forall a h. Array a -> STArray h a

cloneSTMat :: forall r c h a e. (STMat r c h a) -> Eff (st :: ST h | e) (STMat r c h a)
cloneSTMat (STMat arr) = STMat <<< unsafeThaw <$> freeze arr

fromSTMat :: forall r c h a e. Sized r => Sized c => (STMat r c h a) -> Eff (st :: ST h | e) (M.Mat r c a)
fromSTMat (STMat arr) = do
    x   <- freeze arr
    pure (M.fromArrayColumns x)

toSTMat :: forall r c h a e. (M.Mat r c a) -> Eff (st :: ST h | e) (STMat r c h a)
toSTMat m = STMat <$> thaw (M.toArrayColumns m)

-- copyToSTMat :: forall r c h a e. (M.Matrix (M.Mat r c) a) => (M.Mat r c a) -> (STMat r c h a) -> Eff (st :: ST h | e) Unit

foreign import copyToSTMat :: forall r c h a e. (M.Mat r c a) -> (STMat r c h a) -> Eff (st :: ST h | e) Unit

identityST' :: forall r c h e. Sized r => Sized c => Eff (st :: ST h | e) (STMat r c h Number)
identityST' =
    let m = M.identity' :: M.Mat r c Number
    in STMat <$> thaw (M.toArrayColumns m)

foreign import scaleSTMatrixInt :: forall a h e. (EuclideanRing a) => a -> STArray h a -> Eff (st :: ST h | e) Unit

scaleSTMatrix :: forall r c a h e. (EuclideanRing a) => a -> (STMat r c h a) -> Eff (st :: ST h | e) (STMat r c h a)
scaleSTMatrix x v@(STMat arr) = scaleSTMatrixInt x arr *> pure v

fromMatrix :: forall r c a h e. M.Mat r c a -> Eff (st :: ST h | e) (STMat r c h a)
fromMatrix (M.Mat m) = STMat <$> thaw m

foreign import runSTMatrix :: forall r c a e. (forall h. Eff (st :: ST h | e) (STMat r c h a)) -> Eff e (M.Mat r c a)
