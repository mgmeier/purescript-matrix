## Module Data.ST.Matrix4

Inspired by Mjs library for javascript

#### `STMat4`

``` purescript
type STMat4 h = STMat Four h Number
```

#### `identityST`

``` purescript
identityST :: forall h r. Effect (STMat Four h Number)
```

#### `rotateST`

``` purescript
rotateST :: forall h r. Number -> Vec3N -> STMat4 h -> Effect Unit
```

#### `rotateSTX`

``` purescript
rotateSTX :: forall h r. Number -> STMat4 h -> Effect Unit
```

#### `rotateSTY`

``` purescript
rotateSTY :: forall h r. Number -> STMat4 h -> Effect Unit
```

#### `rotateSTZ`

``` purescript
rotateSTZ :: forall h r. Number -> STMat4 h -> Effect Unit
```

#### `translateST`

``` purescript
translateST :: forall h r. Vec3N -> STMat4 h -> Effect Unit
```

#### `scaleST3`

``` purescript
scaleST3 :: forall h r. Number -> Number -> Number -> STMat4 h -> Effect Unit
```


