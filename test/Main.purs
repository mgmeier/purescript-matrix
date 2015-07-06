
module Test.Main where

import Prelude
import qualified Data.Matrix4 as M
import qualified Data.ST.Matrix4 as M
import qualified Data.Matrix as M
import qualified Data.ST.Matrix as M
import qualified Data.Vector as V

import Control.Monad.Eff
import Control.Monad.ST (ST())
import Control.Apply
import Control.Monad.Eff.Console (print)


ble :: forall h r . Eff (st :: ST h | r) (M.STMat4 h)
-- ble = fromMatrix (M.fromArray [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]) >>= stackPush >>= scaleSTMatrix 2 >>= stackPop
-- ble = M.identityST >>= M.scaleSTMatrix 2
ble = M.identityST

meh :: M.Vec3N
meh = V.Vec [0.5,1.5,0.9]

ys :: M.Mat4
ys = M.rotate 90.0 meh $ M.identity

zs :: M.Mat4
zs = M.translate meh $ M.identity

main = do

    xs <- M.runSTMatrix (ble >>= \m -> M.rotateST 90.0 meh m *> return m)
    bs <- M.runSTMatrix (ble >>= \m -> M.translateST meh m *> return m)

    print xs
    print ys

    print bs
    print zs
