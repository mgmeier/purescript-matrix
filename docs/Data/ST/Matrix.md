## Module Data.ST.Matrix

Binding to mjs library

#### `STMat`

``` purescript
newtype STMat r c h a
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
cloneSTMat :: forall r c h a e. (STMat r c h a) -> Eff (st :: ST h | e) (STMat r c h a)
```

#### `fromSTMat`

``` purescript
fromSTMat :: forall r c h a e. Sized r => Sized c => (STMat r c h a) -> Eff (st :: ST h | e) (Mat r c a)
```

#### `toSTMat`

``` purescript
toSTMat :: forall r c h a e. (Mat r c a) -> Eff (st :: ST h | e) (STMat r c h a)
```

#### `copyToSTMat`

``` purescript
copyToSTMat :: forall r c h a e. (Mat r c a) -> (STMat r c h a) -> Eff (st :: ST h | e) Unit
```

#### `identityST'`

``` purescript
identityST' :: forall r c h e. Sized r => Sized c => Eff (st :: ST h | e) (STMat r c h Number)
```

#### `scaleSTMatrixInt`

``` purescript
scaleSTMatrixInt :: forall a h e. EuclideanRing a => a -> STArray h a -> Eff (st :: ST h | e) Unit
```

#### `scaleSTMatrix`

``` purescript
scaleSTMatrix :: forall r c a h e. EuclideanRing a => a -> (STMat r c h a) -> Eff (st :: ST h | e) (STMat r c h a)
```

#### `fromMatrix`

``` purescript
fromMatrix :: forall r c h e a. Mat r c a -> Eff (st :: ST h | e) (STMat r c h a)
```

#### `runSTMatrix`

``` purescript
runSTMatrix :: forall r c a e. (forall h. Eff (st :: ST h | e) (STMat r c h a)) -> Eff e (Mat r c a)
```
