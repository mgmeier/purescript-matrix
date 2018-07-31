-----------------------------------------------------------------------------
--
-- Module      :  ST.Matrix4
-- Copyright   :  Michael Karg
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
-- | Inspired by Mjs library for javascript
--
-----------------------------------------------------------------------------

module Data.ST.Matrix4 where

import Prelude (Unit)
import Data.TypeNat (Four)
import Data.Matrix4 (Vec3N())
import Data.ST.Matrix (STMat)
import Effect (Effect)
import Control.Monad.ST (ST())

type STMat4 h = STMat Four h Number

foreign import identityST  :: forall h. Effect (STMat Four h Number)

foreign import rotateST  :: forall h. Number -> Vec3N -> STMat4 h -> Effect Unit

foreign import rotateSTX  :: forall h. Number -> STMat4 h -> Effect Unit

foreign import rotateSTY  :: forall h. Number -> STMat4 h -> Effect Unit

foreign import rotateSTZ  :: forall h. Number -> STMat4 h -> Effect Unit

-- generic type was :: forall h. Number -> [Number] -> STArray h Number -> Effect Unit

foreign import translateST  :: forall h. Vec3N -> STMat4 h -> Effect Unit

-- generic type was :: forall h. [Number] -> STArray h Number -> Effect Unit

foreign import scaleST3  :: forall h. Number -> Number -> Number -> STMat4 h -> Effect Unit
