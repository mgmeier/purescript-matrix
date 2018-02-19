
module Test.Main where

import Prelude

import Test.Spec (describe, it)
import Test.Spec.Assertions as A
import Test.Spec.Assertions.String as S
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import Data.TypeNat (Four, Three)

import Data.Array (length)
import Data.Int (toNumber)
import Data.String (take)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.Vector as V
import Data.Matrix (Mat, columns, fromArrayColumns, generate, getElem, index, mulmatmat, mulmatvec, mulvecmat, rows, show', toArrayColumns, unsafeGetElem) as M
import Data.Matrix4 (Mat4, Vec3N, identity, makeScale, makeTranslate3, rotate, translate) as M
import Data.ST.Matrix (runSTMatrix) as M
import Data.ST.Matrix4 (STMat4, translateST, rotateST, identityST) as M


import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST())
import Control.Monad.Eff.Console (logShow, CONSOLE)

ble :: forall h r . Eff (st :: ST h | r) (M.STMat4 h)
-- ble = fromMatrix (M.fromArrayColumns [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]) >>= stackPush >>= scaleSTMatrix 2 >>= stackPop
-- ble = M.identityST >>= M.scaleSTMatrix 2
ble = M.identityST

meh :: M.Vec3N
meh = V.Vec [0.5,1.5,0.9]

ys :: M.Mat4
ys = M.rotate 90.0 meh $ M.identity

zs :: M.Mat4
zs = M.translate meh $ M.identity

preexisting :: forall e. Eff ( console :: CONSOLE | e ) Unit
preexisting = do
  xs <- M.runSTMatrix (ble >>= \m -> M.rotateST 90.0 meh m *> pure m)
  bs <- M.runSTMatrix (ble >>= \m -> M.translateST meh m *> pure m)

  logShow xs
  logShow ys

  logShow bs
  logShow zs

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "columns" do
    describe "for a given matrix" do
      let m = M.generate (\i j -> toNumber j) :: M.Mat Three Four Number
      let result = M.columns m
      it "gives a row worth of columns" do
        A.shouldEqual (length result) 4

  describe "rows" do
    describe "for a given matrix" do
      let m = M.generate (\i j -> toNumber i) :: M.Mat Three Four Number
      let result = M.rows m
      it "gives a column worth of rows" do
        A.shouldEqual (length result) 3

  describe "index" do
    let m = M.generate (\i j -> 10.0) :: M.Mat Three Four Number
    it "gives zero for zero" do
      A.shouldEqual 0 $ M.index m 0 0
    it "walks i" do
      A.shouldEqual 1 $ M.index m 1 0
    it "wraps good " do
      A.shouldEqual 3 $ M.index m 0 1

  describe "show'" do
    describe "for a given matrix" do
      let m = M.generate (\i j -> 10.0) :: M.Mat Three Four Number
      let result = M.show' m
      it "produces a matrix type header" do
        A.shouldEqual (take 3 result) "Mat"
      it "gives rows x colums" do
        S.shouldContain result "3x4"

  describe "generate" do
    describe "for a given matrix" do
      let result = M.generate (\i j -> i /\ j) :: M.Mat Three Four (Tuple Int Int)
      it "is zero indexed" do
        A.shouldContain (M.toArrayColumns result) (0 /\ 0)
      it "does not invert rows and columns" do
        A.shouldContain (M.toArrayColumns result) (2 /\ 3)
        A.shouldNotContain (M.toArrayColumns result) (3 /\ 2)

  describe "unsafeGetElem" do
    describe "for a given matrix" do
      let m = M.generate (\i j -> i /\ j) :: M.Mat Three Four (Tuple Int Int)
      describe "for an index inside the matrix" do
        it "returns the correct element" do
          A.shouldEqual (M.unsafeGetElem m 2 3) (2 /\ 3)

  describe "mulmatvec" do
    describe "for a scale matrix" do
      let m = M.makeScale $ V.fromArray [3.0, 4.0, 5.0]
      describe "for a given vector" do
        let v = V.fromArray [1.0, 2.0, 3.0, 0.0] :: V.Vec Four Number
        let result = M.mulmatvec m v
        it "performs a scale" do
          -- AC.logShow result
          A.shouldEqual (V.fromArray [3.0, 8.0, 15.0, 0.0]) result

  describe "mulvecmat" do
    describe "for a scale matrix" do
      let m = M.makeScale $ V.fromArray [3.0, 4.0, 5.0]
      describe "for a given vector" do
        let v = V.fromArray [1.0, 2.0, 3.0, 0.0] :: V.Vec Four Number
        let result = M.mulvecmat v m
        it "performs a scale" do
          -- AC.logShow result
          A.shouldEqual (V.fromArray [3.0, 8.0, 15.0, 0.0]) result

  describe "mulmatmat" do
    describe "for a scale matrix" do
      let scale = 3.0
      let mScale = M.makeScale $ V.fromArray [1.0, 2.0, 3.0]
      describe "for a translation matrix" do
        let mTranslate = M.makeTranslate3 10.0 20.0 30.0
        let result = M.mulmatmat mTranslate mScale
        it "performs scale then translation" do
          A.shouldEqual result $ M.fromArrayColumns [1.0,0.0,0.0,0.0, 0.0,2.0,0.0,0.0, 0.0,0.0,3.0,0.0, 10.0, 20.0, 30.0, 1.0]
