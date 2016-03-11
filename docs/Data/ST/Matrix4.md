## Module Data.ST.Matrix4

Inspired by Mjs library for javascript

#### `STMat4`

``` purescript
type STMat4 h = STMat Four h Number
```

#### `identityST`

``` purescript
identityST :: forall h r. Eff (st :: ST h | r) (STMat Four h Number)
```

#### `rotateST`

``` purescript
rotateST :: forall h r. Number -> Vec3N -> STMat4 h -> Eff (st :: ST h | r) Unit
```

#### `rotateSTX`

``` purescript
rotateSTX :: forall h r. Number -> STMat4 h -> Eff (st :: ST h | r) Unit
```

#### `rotateSTY`

``` purescript
rotateSTY :: forall h r. Number -> STMat4 h -> Eff (st :: ST h | r) Unit
```

#### `rotateSTZ`

``` purescript
rotateSTZ :: forall h r. Number -> STMat4 h -> Eff (st :: ST h | r) Unit
```

#### `translateST`

``` purescript
translateST :: forall h r. Vec3N -> STMat4 h -> Eff (st :: ST h | r) Unit
```

#### `scaleST3`

``` purescript
scaleST3 :: forall h r. Number -> Number -> Number -> STMat4 h -> Eff (st :: ST h | r) Unit
```


