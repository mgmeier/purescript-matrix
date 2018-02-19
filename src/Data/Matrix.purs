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

import Data.Array (concat, length, range, slice, zipWith, (!!))
import Data.FunctorWithIndex(class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (over, class Newtype)
import Data.String (joinWith)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested ((/\))
import Data.TypeNat (class Sized, Four, Three, Two, One, sized)
import Data.Vector as V

import Extensions (fail)
import Partial.Unsafe (unsafePartial)

newtype Mat r c a = Mat (Array a)

derive instance newtypeMat :: Newtype (Mat r c a) _

dim :: forall r c a. Sized r => Sized c => Mat r c a -> Tuple Int Int
dim _ = (sized (Proxy :: Proxy r)) /\ (sized (Proxy :: Proxy c))

show' :: forall r c a. Sized r => Sized c => Show a => Mat r c a -> String
show' m = joinWith " " [t, show(columns m)]
  where
    t = joinWith "" ["Mat", show $ fst $ dim m, "x", show $ snd $ dim m]

instance showMat11 :: Show a => Show (Mat One One a) where show = show'

instance showMat12 :: Show a => Show (Mat One Two a) where show = show'
instance showMat21 :: Show a => Show (Mat Two One a) where show = show'
instance showMat22 :: Show a => Show (Mat Two Two a) where show = show'

instance showMat31 :: Show a => Show (Mat Three One a) where show = show'
instance showMat13 :: Show a => Show (Mat One Three a) where show = show'
instance showMat32 :: Show a => Show (Mat Three Two a) where show = show'
instance showMat23 :: Show a => Show (Mat Two Three a) where show = show'
instance showMat33 :: Show a => Show (Mat Three Three a) where show = show'

instance showMat41 :: Show a => Show (Mat Four One a) where show = show'
instance showMat14 :: Show a => Show (Mat One Four a) where show = show'
instance showMat42 :: Show a => Show (Mat Four Two a) where show = show'
instance showMat24 :: Show a => Show (Mat Two Four a) where show = show'
instance showMat43 :: Show a => Show (Mat Four Three a) where show = show'
instance showMat34 :: Show a => Show (Mat Three Four a) where show = show'
instance showMat44 :: Show a => Show (Mat Four Four a) where show = show'

instance eqMat :: (Eq a) => Eq (Mat r c a) where
  eq (Mat l) (Mat r) = l == r

instance functorMat :: Functor (Mat r c) where
  map f (Mat l) = Mat (map f l)

instance functorWithIndexMat :: FunctorWithIndex Int (Mat r c) where
  mapWithIndex f =  over Mat (mapWithIndex f)
-- instance functorWithIndexMat :: FunctorWithIndex Int (Mat r c) where
--   mapWithIndex f (Mat l) =  Mat (mapWithIndex f l)

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

rows :: forall r c a. Sized r => Sized c => Mat r c a -> Array (V.Vec c a)
rows = columns <<< transpose

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
  -> Maybe a
getElem m@(Mat l) i j = l !! index m i j

unsafeGetElem :: forall r c a.
  Sized r => Sized c =>
  Mat r c a     -- Matrix
  -> Int        -- Row
  -> Int        -- Column
  -> a
unsafeGetElem m@(Mat l) i j = unsafePartial $ fromJust (l !! index m i j)

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

fromArrayRows :: forall r c a. Sized r => Sized c => Array a -> Mat r c a
fromArrayRows = transpose <<< fromArrayColumns

toArrayColumns :: forall r c a. Mat r c a -> Array a
toArrayColumns (Mat a) = a

toArrayRows :: forall r c a. Sized r => Sized c => Mat r c a -> Array a
toArrayRows = toArrayColumns <<< transpose

fromColumns :: forall r c a. Sized r => Sized c => Array (V.Vec r a) -> Mat r c a
fromColumns vs | ((length vs) == sized (Proxy :: Proxy c)) = Mat $ concat $ V.toArray <$> vs
fromColumns _ = fail "Matrix>>fromColumns: Wrong array length!"

fromRows :: forall r c a. Sized r => Sized c => Array (V.Vec c a) -> Mat r c a
fromRows vs | ((length vs) == sized (Proxy :: Proxy r)) = transpose $ Mat $ concat $ V.toArray <$> vs
fromRows _ = fail "Matrix>>fromColumns: Wrong array length!"

-- | /O(rows*cols)/. The transpose of a matrix.
--   Example:
--
-- >           ( 1 2 3 )   ( 1 4 7 )
-- >           ( 4 5 6 )   ( 2 5 8 )
-- > transpose ( 7 8 9 ) = ( 3 6 9 )
transpose :: forall r c a.  Sized r => Sized c => Mat r c a -> Mat c r a
transpose m = generate $ \i j -> unsafeGetElem m j i

mulmatvec :: forall r c. Sized r => Sized c => Mat r c Number -> V.Vec c Number -> V.Vec r Number
mulmatvec m v = V.Vec $ V.dot <$> rows m <*> [v]

mulvecmat :: forall r c. Sized r => Sized c => V.Vec r Number -> Mat r c Number -> V.Vec c Number
mulvecmat v m = V.Vec $ V.dot <$> [v] <*> columns m

mulmatmat :: forall rl u cr. Sized rl => Sized u => Sized cr => Mat rl u Number -> Mat u cr Number -> Mat rl cr Number
mulmatmat l r = fromColumns $ (mulmatvec l <$> columns r)


{-
instance foldableMat :: Foldable (Mat r c) where
  foldr f z (Vec xs) = foldr f z xs
  foldl f z (Vec xs) = foldl f z xs
  foldMap f xs = foldr (\x acc -> f x <> acc) mempty xs
-}
