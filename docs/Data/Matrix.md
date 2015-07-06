## Module Data.Matrix

#### `Mat`

``` purescript
newtype Mat s a
  = Mat (Array a)
```

##### Instances
``` purescript
instance showMat2 :: (Show a) => Show (Mat (Suc (Suc Zero)) a)
instance showMat3 :: (Show a) => Show (Mat (Suc (Suc (Suc Zero))) a)
instance showMat4 :: (Show a) => Show (Mat (Suc (Suc (Suc (Suc Zero)))) a)
instance eqMat :: (Eq a) => Eq (Mat s a)
instance functorMat :: Functor (Mat s)
instance applyMat :: Apply (Mat s)
```

#### `generate`

``` purescript
generate :: forall a s. (Sized s) => (Int -> Int -> a) -> Mat s a
```

#### `columns`

``` purescript
columns :: forall s a. (Sized s) => Mat s a -> Array (Array a)
```

#### `identity'`

``` purescript
identity' :: forall s a. (Sized s) => Mat s Number
```

#### `getElem`

``` purescript
getElem :: forall s a. (Sized s) => Int -> Int -> Mat s a -> a
```

/O(1)/. Get an element of a matrix.

#### `scaleMatrix`

``` purescript
scaleMatrix :: forall a s. (Num a) => a -> Mat s a -> Mat s a
```

#### `fromArray`

``` purescript
fromArray :: forall a s. (Sized s) => Array a -> Mat s a
```

#### `toArray`

``` purescript
toArray :: forall s a. Mat s a -> Array a
```

#### `transpose`

``` purescript
transpose :: forall a s. (Sized s) => Mat s a -> Mat s a
```


