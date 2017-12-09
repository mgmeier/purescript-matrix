## Module Data.Matrix

Binding to mjs library

#### `Mat`

``` purescript
newtype Mat r c a
  = Mat (Array a)
```

##### Instances
``` purescript
(Show a) => Show (Mat Two a)
(Show a) => Show (Mat Three a)
(Show a) => Show (Mat Four a)
(Eq a) => Eq (Mat r c a)
Functor (Mat r c)
Apply (Mat r c)
```

#### `generate`

``` purescript
generate :: forall a r c. Sized r => Sized c => (Int -> Int -> a) -> Mat r c a
```

/O(rows*cols)/. Generate a matrix from a generator function.

#### `columns`

``` purescript
columns :: forall r c a. Sized r => Sized c => Mat r c a -> Array (Array a)
```

#### `identity'`

``` purescript
identity' :: forall r c. Sized r => Sized c => Mat r c Number
```

/O(rows*cols)/. Identity matrix of the given order.

#### `getElem`

``` purescript
getElem :: forall r c a. Sized r => Sized c => Int -> Int -> Mat r c a -> a
```

/O(1)/. Get an element of a matrix.

#### `scaleMatrix`

``` purescript
scaleMatrix :: forall a r c. EuclideanRing a => a -> Mat r c a -> Mat r c a
```

Scale a matrix by a given factor.

#### `fromArrayColumns`

``` purescript
fromArrayColumns :: forall a r c. Sized r => Sized c => Array a -> Mat r c a
```

#### `toArrayColumns`

``` purescript
toArrayColumns :: forall r c a. Mat r c a -> Array a
```

#### `transpose`

``` purescript
transpose :: forall a r c. Sized r => Sized c => Mat r c a -> Mat r c a
```

/O(rows*cols)/. The transpose of a matrix.
