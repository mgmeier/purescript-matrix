## Module Data.ST.Matrix

Binding to mjs library

#### `STMat`

``` purescript
newtype STMat s h a
  = STMat (STArray h a)
```

#### `copyImpl`

``` purescript
copyImpl :: forall a b h. a -> Effect b
```

#### `freeze`

``` purescript
freeze :: forall a h. STArray h a -> Effect (Array a)
```

Create an immutable copy of a mutable array.

#### `thaw`

``` purescript
thaw :: forall a h. Array a -> Effect (STArray h a)
```

Create a mutable copy of an immutable array.

#### `unsafeFreeze`

``` purescript
unsafeFreeze :: forall a h. STArray h a -> Array a
```

Freeze an ST array. Do not mutate the STArray afterwards!

#### `unsafeThaw`

``` purescript
unsafeThaw :: forall a h. Array a -> STArray h a
```

#### `cloneSTMat`

``` purescript
cloneSTMat :: forall s h a. (STMat s h a) -> Effect (STMat s h a)
```

#### `fromSTMat`

``` purescript
fromSTMat :: forall s h a. Sized s => (STMat s h a) -> Effect (Mat s a)
```

#### `toSTMat`

``` purescript
toSTMat :: forall s h a. (Mat s a) -> Effect (STMat s h a)
```

#### `copyToSTMat`

``` purescript
copyToSTMat :: forall s h a. (Mat s a) -> (STMat s h a) -> Effect Unit
```

#### `identityST'`

``` purescript
identityST' :: forall s h. Sized s => Effect (STMat s h Number)
```

#### `scaleSTMatrixInt`

``` purescript
scaleSTMatrixInt :: forall a h. EuclideanRing a => a -> STArray h a -> Effect Unit
```

#### `scaleSTMatrix`

``` purescript
scaleSTMatrix :: forall s a h. EuclideanRing a => a -> (STMat s h a) -> Effect (STMat s h a)
```

#### `fromMatrix`

``` purescript
fromMatrix :: forall s h a. Mat s a -> Effect (STMat s h a)
```

#### `runSTMatrix`

``` purescript
runSTMatrix :: forall s a. (forall h. Effect (STMat s h a)) -> Effect (Mat s a)
```


