## Module Data.ST.Matrix4

Inspired by Mjs library for javascript

#### `STMat4`

``` purescript
type STMat4 h = STMat Four h Number
```

#### `identityST`

``` purescript
identityST :: forall h e. Eff (st :: ST h | e) (STMat Four h Number)
```

#### `rotateST`

``` purescript
rotateST :: forall h e. Number -> Vec3N -> STMat4 h -> Eff (st :: ST h | e) Unit
```

#### `rotateSTX`

``` purescript
rotateSTX :: forall h e. Number -> STMat4 h -> Eff (st :: ST h | e) Unit
```

#### `rotateSTY`

``` purescript
rotateSTY :: forall h e. Number -> STMat4 h -> Eff (st :: ST h | e) Unit
```

#### `rotateSTZ`

``` purescript
rotateSTZ :: forall h e. Number -> STMat4 h -> Eff (st :: ST h | e) Unit
```

#### `translateST`

``` purescript
translateST :: forall h e. Vec3N -> STMat4 h -> Eff (st :: ST h | e) Unit
```

#### `scaleST3`

``` purescript
scaleST3 :: forall h e. Number -> Number -> Number -> STMat4 h -> Eff (st :: ST h | e) Unit
```
