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
import Data.Array (length, (!!), zipWith, slice, range, concat)
import Data.Maybe (fromJust)
import Data.TypeNat (class Sized, Four, Three, Two, sized)
import Type.Proxy (Proxy(Proxy))
import Extensions (fail)
import Partial.Unsafe (unsafePartial)

newtype Mat s a = Mat (Array a)

-- | /O(rows*cols)/. Generate a matrix from a generator function.
--   Example of usage:
--
-- >                                  (  1  0 -1 -2 )
-- >                                  (  3  2  1  0 )
-- >                                  (  5  4  3  2 )
-- > (generate $ \ i j -> 2*i - j) :: Mat Four Number = (  7  6  5  4 )
generate :: forall a s. (Sized s) =>
          (Int -> Int -> a) -- ^ Generator function
            -> Mat s a
generate f =
    let size = sized (Proxy :: Proxy s)
    in Mat $ concat $
        (\col -> (\row -> f col row)  <$> (range 0 (size - 1)))
            <$> (range 0 (size - 1))

instance showMat2 :: (Show a) => Show (Mat Two a) where
  show m = "Mat2x2 " <> show (columns m)
instance showMat3 :: (Show a) => Show (Mat Three a) where
  show m = "Mat3x3 " <> show (columns m)
instance showMat4 :: (Show a) => Show (Mat Four a) where
  show m = "Mat4x4 " <> show (columns m)

columns :: forall s a . (Sized s) => Mat s a -> Array (Array a)
columns mat@(Mat m) | sized (Proxy :: Proxy s) == 2 =
    [slice 0 2 m,
     slice 2 4 m]
                    | sized (Proxy :: Proxy s) == 3 =
    [slice 0 3 m,
     slice 3 6 m,
     slice 6 9 m]
                    | sized (Proxy :: Proxy s) == 4 =
  [slice 0 4 m,
   slice 4 8 m,
   slice 8 12 m,
   slice 12 16 m]
                    | otherwise                        =
    fail "Matrix>>columns: Proxy size not supprted!"

instance eqMat :: (Eq a) => Eq (Mat s a) where
  eq (Mat l) (Mat r) = l == r

instance functorMat :: Functor (Mat s) where
  map f (Mat l) = Mat (map f l)

instance applyMat :: Apply (Mat s) where
  apply (Mat f) (Mat a) = Mat (zipWith (\f' a' -> f' a') f a)

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
identity' :: forall s.  (Sized s) => Mat s Number
identity' = generate \ i j -> if i == j then 1.0 else 0.0

-- | /O(1)/. Get an element of a matrix.
getElem :: forall s a. (Sized s) =>
           Int      -- ^ Row
        -> Int      -- ^ Column
        -> Mat s a     -- ^ Matrix
        -> a
getElem i j m@(Mat l) = unsafePartial $ fromJust (l !! (i * sized (Proxy :: Proxy s) + j))

-- | Scale a matrix by a given factor.
--   Example:
--
-- >               ( 1 2 3 )   (  2  4  6 )
-- >               ( 4 5 6 )   (  8 10 12 )
-- > scaleMatrix 2 ( 7 8 9 ) = ( 14 16 18 )
scaleMatrix ::  forall a s. (EuclideanRing a) => a -> Mat s a -> Mat s a
scaleMatrix = (<$>) <<< (*)

fromArrayColumns :: forall a s. (Sized s) => Array a -> Mat s a
fromArrayColumns l =
  let size = sized (Proxy :: Proxy s)
  in case size * size of
        i | i == length l -> Mat l
          | otherwise     -> fail "Matrix>>fromArrayColumns: Wrong array length!"

toArrayColumns :: forall s a. Mat s a -> Array a
toArrayColumns (Mat a) = a

-- | /O(rows*cols)/. The transpose of a matrix.
--   Example:
--
-- >           ( 1 2 3 )   ( 1 4 7 )
-- >           ( 4 5 6 )   ( 2 5 8 )
-- > transpose ( 7 8 9 ) = ( 3 6 9 )
transpose :: forall a s.  (Sized s) => Mat s a -> Mat s a
transpose m = generate $ \ i j -> getElem j i m


{-
instance foldableMat :: Foldable (Mat s) where
  foldr f z (Vec xs) = foldr f z xs
  foldl f z (Vec xs) = foldl f z xs
  foldMap f xs = foldr (\x acc -> f x <> acc) mempty xs
-}
