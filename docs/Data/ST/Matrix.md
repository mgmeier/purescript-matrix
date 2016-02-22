## Module Data.ST.Matrix

Binding to mjs library

#### `STMat`

``` purescript
newtype STMat s h a
  = STMat (STArray h a)
```

#### `copyImpl`

``` purescript
copyImpl :: forall a b h r. a -> Eff (st :: ST h | r) b
```

#### `freeze`

``` purescript
freeze :: forall a h r. STArray h a -> Eff (st :: ST h | r) (Array a)
```

Create an immutable copy of a mutable array.

#### `thaw`

``` purescript
thaw :: forall a h r. Array a -> Eff (st :: ST h | r) (STArray h a)
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
cloneSTMat :: forall s h a r. STMat s h a -> Eff (st :: ST h | r) (STMat s h a)
```

#### `fromSTMat`

``` purescript
fromSTMat :: forall s h a r. (Sized s) => STMat s h a -> Eff (st :: ST h | r) (Mat s a)
```

#### `toSTMat`

``` purescript
toSTMat :: forall s h a r. Mat s a -> Eff (st :: ST h | r) (STMat s h a)
```

#### `copyToSTMat`

``` purescript
copyToSTMat :: forall s h a r. Mat s a -> STMat s h a -> Eff (st :: ST h | r) Unit
```

#### `identityST'`

``` purescript
identityST' :: forall s h r. (Sized s) => Eff (st :: ST h | r) (STMat s h Number)
```

#### `scaleSTMatrixInt`

``` purescript
scaleSTMatrixInt :: forall a h r. (Num a) => a -> STArray h a -> Eff (st :: ST h | r) Unit
```

#### `scaleSTMatrix`

``` purescript
scaleSTMatrix :: forall s a h r. (Num a) => a -> STMat s h a -> Eff (st :: ST h | r) (STMat s h a)
```

#### `fromMatrix`

``` purescript
fromMatrix :: forall s h r a. Mat s a -> Eff (st :: ST h | r) (STMat s h a)
```

#### `runSTMatrix`

``` purescript
runSTMatrix :: forall s a r. (forall h. Eff (st :: ST h | r) (STMat s h a)) -> Eff r (Mat s a)
```


