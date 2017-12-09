## Module Data.Matrix

Binding to mjs library

#### `Mat`

``` purescript
newtype Mat s a
  = Mat (Array a)
```

##### Instances
``` purescript
(Show a) => Show (Mat Two a)
(Show a) => Show (Mat Three a)
(Show a) => Show (Mat Four a)
(Eq a) => Eq (Mat s a)
Functor (Mat s)
Apply (Mat s)
```

#### `generate`

``` purescript
generate :: forall a s. Sized s => (Int -> Int -> a) -> Mat s a
```

/O(rows*cols)/. Generate a matrix from a generator function.

#### `columns`

``` purescript
columns :: forall s a. Sized s => Mat s a -> Array (Array a)
```

#### `identity'`

``` purescript
identity' :: forall s. Sized s => Mat s Number
```

/O(rows*cols)/. Identity matrix of the given order.

#### `getElem`

``` purescript
getElem :: forall s a. Sized s => Int -> Int -> Mat s a -> a
```

/O(1)/. Get an element of a matrix.

#### `scaleMatrix`

``` purescript
scaleMatrix :: forall a s. EuclideanRing a => a -> Mat s a -> Mat s a
```

Scale a matrix by a given factor.

#### `fromArrayColumns`

``` purescript
fromArrayColumns :: forall a s. Sized s => Array a -> Mat s a
```

#### `toArray`

``` purescript
toArray :: forall s a. Mat s a -> Array a
```

#### `transpose`

``` purescript
transpose :: forall a s. Sized s => Mat s a -> Mat s a
```

/O(rows*cols)/. The transpose of a matrix.
