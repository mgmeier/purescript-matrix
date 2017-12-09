-----------------------------------------------------------------------------
--
-- Module      :  Matrix
-- Copyright   :  Jürgen Nicklisch-Franken
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
-- | Binding to mjs library
--
-----------------------------------------------------------------------------

module Data.Matrix where

import Prelude
import Type.Proxy (Proxy(Proxy))

import Data.Array (length, range, slice, zipWith, (!!))
import Data.Maybe (fromJust)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Data.TypeNat (class Sized, Four, Three, Two, sized)
import Data.Vector as V

import Extensions (fail)
import Partial.Unsafe (unsafePartial)

newtype Mat r c a = Mat (Array a)

instance showMat2 :: (Show a) => Show (Mat Two Two a) where
  show m = "Mat2x2 " <> show (columns m)
instance showMat3 :: (Show a) => Show (Mat Three Three a) where
  show m = "Mat3x3 " <> show (columns m)
instance showMat4 :: (Show a) => Show (Mat Four Four a) where
  show m = "Mat4x4 " <> show (columns m)

dim :: forall r c a. Sized r => Sized c => Mat r c a -> Tuple Int Int
dim _ = (sized (Proxy :: Proxy r)) /\ (sized (Proxy :: Proxy c))


instance eqMat :: (Eq a) => Eq (Mat r c a) where
  eq (Mat l) (Mat r) = l == r

instance functorMat :: Functor (Mat r c) where
  map f (Mat l) = Mat (map f l)

instance applyMat :: Apply (Mat r c) where
  apply (Mat f) (Mat a) = Mat (zipWith (\f' a' -> f' a') f a)

-- | /O(rows*cols)/. Generate a matrix from a generator function.
--   Example of usage:
--
-- >                                  (  1  0 -1 -2 )
-- >                                  (  3  2  1  0 )
-- >                                  (  5  4  3  2 )
-- > (generate $ \ i j -> 2*i - j) :: Mat Four Four Number = ( 7  6  5  4 )
generate :: forall r c a. Sized r => Sized c =>
          (Int -> Int -> a) -- ^ Generator function
            -> Mat r c a
generate f =
    Mat do
      col <- range 0 (cs - 1)
      row <- range 0 (rs - 1)
      pure $ f row col
    where
      -- determine rows and cols without a Mat value
      cs = sized (Proxy :: Proxy c)
      rs = sized (Proxy :: Proxy r)

columns :: forall r c a . Sized r => Sized c => Mat r c a -> Array (V.Vec r a)
columns m@(Mat a) = V.Vec <$> (slicers <*> [a])
  where
    slicers = do
      let row = fst $ dim m
      col <- range 0 ((snd $ dim m) - 1)
      pure $ slice (col * row) ((col + 1) * row)

-- | /O(rows*cols)/. Identity matrix of the given order.
--
-- > identity n =
-- >                 n
-- >   1 ( 1 0 ... 0 0 )
-- >   2 ( 0 1 ... 0 0 )
-- >     (     ...     )
-- >     ( 0 0 ... 1 0 )
-- >   n ( 0 0 ... 0 1 )
--
identity' :: forall r c. Sized r => Sized c => Mat r c Number
identity' = generate \ i j -> if i == j then 1.0 else 0.0

index :: forall r c a. Sized r => Sized c => Mat r c a -> Int -> Int -> Int
index m i j = (j * (fst $ dim m) + i)

-- | /O(1)/. Get an element of a matrix.
getElem :: forall r c a.
  Sized r => Sized c =>
  Mat r c a     -- Matrix
  -> Int        -- Row
  -> Int        -- Column
  -> a
getElem m@(Mat l) i j = unsafePartial $ fromJust (l !! index m i j)

-- | Scale a matrix by a given factor.
--   Example:
--
-- >               ( 1 2 3 )   (  2  4  6 )
-- >               ( 4 5 6 )   (  8 10 12 )
-- > scaleMatrix 2 ( 7 8 9 ) = ( 14 16 18 )
scaleMatrix ::  forall r c a. (EuclideanRing a) => a -> Mat r c a -> Mat r c a
scaleMatrix = (<$>) <<< (*)

fromArrayColumns :: forall r c a. Sized r => Sized c => Array a -> Mat r c a
fromArrayColumns l =
  case cs * rs of
        i | i == length l -> Mat l
          | otherwise     -> fail "Matrix>>fromArrayColumns: Wrong array length!"
  where
    cs = sized (Proxy :: Proxy c)
    rs = sized (Proxy :: Proxy r)

toArrayColumns :: forall r c a. Mat r c a -> Array a
toArrayColumns (Mat a) = a

-- | /O(rows*cols)/. The transpose of a matrix.
--   Example:
--
-- >           ( 1 2 3 )   ( 1 4 7 )
-- >           ( 4 5 6 )   ( 2 5 8 )
-- > transpose ( 7 8 9 ) = ( 3 6 9 )
transpose :: forall r c a.  Sized r => Sized c => Mat r c a -> Mat c r a
transpose m = generate $ \i j -> getElem m j i



{-
instance foldableMat :: Foldable (Mat r c) where
  foldr f z (Vec xs) = foldr f z xs
  foldl f z (Vec xs) = foldl f z xs
  foldMap f xs = foldr (\x acc -> f x <> acc) mempty xs
-}
