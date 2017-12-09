## Module Data.ST.Matrix

Binding to mjs library

#### `STMat`

``` purescript
newtype STMat s h a
  = STMat (STArray h a)
```

#### `copyImpl`

``` purescript
copyImpl :: forall a b h e. a -> Eff (st :: ST h | e) b
```

#### `freeze`

``` purescript
freeze :: forall a h e. STArray h a -> Eff (st :: ST h | e) (Array a)
```

Create an immutable copy of a mutable array.

#### `thaw`

``` purescript
thaw :: forall a h e. Array a -> Eff (st :: ST h | e) (STArray h a)
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
cloneSTMat :: forall s h a e. (STMat s h a) -> Eff (st :: ST h | e) (STMat s h a)
```

#### `fromSTMat`

``` purescript
fromSTMat :: forall s h a e. Sized s => (STMat s h a) -> Eff (st :: ST h | e) (Mat s a)
```

#### `toSTMat`

``` purescript
toSTMat :: forall s h a e. (Mat s a) -> Eff (st :: ST h | e) (STMat s h a)
```

#### `copyToSTMat`

``` purescript
copyToSTMat :: forall s h a e. (Mat s a) -> (STMat s h a) -> Eff (st :: ST h | e) Unit
```

#### `identityST'`

``` purescript
identityST' :: forall s h e. Sized s => Eff (st :: ST h | e) (STMat s h Number)
```

#### `scaleSTMatrixInt`

``` purescript
scaleSTMatrixInt :: forall a h e. EuclideanRing a => a -> STArray h a -> Eff (st :: ST h | e) Unit
```

#### `scaleSTMatrix`

``` purescript
scaleSTMatrix :: forall s a h e. EuclideanRing a => a -> (STMat s h a) -> Eff (st :: ST h | e) (STMat s h a)
```

#### `fromMatrix`

``` purescript
fromMatrix :: forall s h e a. Mat s a -> Eff (st :: ST h | e) (STMat s h a)
```

#### `runSTMatrix`

``` purescript
runSTMatrix :: forall s a e. (forall h. Eff (st :: ST h | e) (STMat s h a)) -> Eff e (Mat s a)
```
