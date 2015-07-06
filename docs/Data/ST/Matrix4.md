## Module Data.ST.Matrix4

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

#### `translateST`

``` purescript
translateST :: forall h r. Vec3N -> STMat4 h -> Eff (st :: ST h | r) Unit
```

#### `scaleST3`

``` purescript
scaleST3 :: forall h r. Number -> Number -> Number -> STMat4 h -> Eff (st :: ST h | r) Unit
```


