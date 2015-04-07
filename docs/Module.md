# Module Documentation

## Module Data.Matrix


#### `Mat`

``` purescript
newtype Mat s a
  = Mat [a]
```


#### `sm2`

``` purescript
instance sm2 :: Sized (Mat Two a)
```


#### `sm3`

``` purescript
instance sm3 :: Sized (Mat Three a)
```


#### `sm4`

``` purescript
instance sm4 :: Sized (Mat Four a)
```


#### `Matrix`

``` purescript
class (Sized (m a)) <= Matrix m a where
  generate :: (Number -> Number -> a) -> m a
```


#### `m2`

``` purescript
instance m2 :: Matrix (Mat Two) a
```


#### `m3`

``` purescript
instance m3 :: Matrix (Mat Three) a
```


#### `m4`

``` purescript
instance m4 :: Matrix (Mat Four) a
```


#### `generate_`

``` purescript
generate_ :: forall a s. Number -> (Number -> Number -> a) -> Mat s a
```

#### `showMat2`

``` purescript
instance showMat2 :: (Show a) => Show (Mat Two a)
```


#### `showMat3`

``` purescript
instance showMat3 :: (Show a) => Show (Mat Three a)
```


#### `showMat4`

``` purescript
instance showMat4 :: (Show a) => Show (Mat Four a)
```


#### `columns`

``` purescript
columns :: forall s a. (Matrix (Mat s) a) => Mat s a -> [[a]]
```


#### `eqMat`

``` purescript
instance eqMat :: (Eq a) => Eq (Mat s a)
```


#### `functorMat`

``` purescript
instance functorMat :: Functor (Mat s)
```


#### `applyMat`

``` purescript
instance applyMat :: Apply (Mat s)
```


#### `identity'`

``` purescript
identity' :: forall s a. (Matrix (Mat s) Number) => Mat s Number
```

#### `getElem`

``` purescript
getElem :: forall s a. (Matrix (Mat s) a) => Number -> Number -> Mat s a -> a
```

/O(1)/. Get an element of a matrix.

#### `scaleMatrix`

``` purescript
scaleMatrix :: forall a s. (Matrix (Mat s) a, Num a) => a -> Mat s a -> Mat s a
```

#### `fromArray`

``` purescript
fromArray :: forall a s. (Matrix (Mat s) a) => [a] -> Mat s a
```


#### `toArray`

``` purescript
toArray :: forall s a. Mat s a -> [a]
```


#### `transpose`

``` purescript
transpose :: forall a s. (Matrix (Mat s) a) => Mat s a -> Mat s a
```


## Module Data.Matrix3


#### `Mat3`

``` purescript
type Mat3 = Mat Three Number
```


#### `mat3`

``` purescript
mat3 :: [Number] -> Mat3
```


#### `identity`

``` purescript
identity :: Mat3
```


#### `normalFromMat4`

``` purescript
normalFromMat4 :: Mat Four Number -> Maybe Mat3
```



## Module Data.Matrix4


#### `Vec3N`

``` purescript
type Vec3N = V3.Vec3 Number
```


#### `Mat4`

``` purescript
type Mat4 = Mat Four Number
```


#### `mat4`

``` purescript
mat4 :: [Number] -> Mat4
```


#### `identity`

``` purescript
identity :: Mat4
```


#### `transform`

``` purescript
transform :: Mat4 -> Vec3N -> Vec3N
```

Multiply a V.Vector by a 4x4 matrix: m * v

#### `inverseOrthonormal`

``` purescript
inverseOrthonormal :: Mat4 -> Mat4
```

#### `inverse`

``` purescript
inverse :: Mat4 -> Maybe Mat4
```

#### `makeFrustum`

``` purescript
makeFrustum :: Number -> Number -> Number -> Number -> Number -> Number -> Mat4
```

#### `makePerspective`

``` purescript
makePerspective :: Number -> Number -> Number -> Number -> Mat4
```

#### `makeOrtho`

``` purescript
makeOrtho :: Number -> Number -> Number -> Number -> Number -> Number -> Mat4
```

#### `makeOrtho2D`

``` purescript
makeOrtho2D :: Number -> Number -> Number -> Number -> Mat4
```

#### `mul`

``` purescript
mul :: Mat4 -> Mat4 -> Mat4
```

Matrix multiplcation: a * b

#### `mulAffine`

``` purescript
mulAffine :: Mat4 -> Mat4 -> Mat4
```

Matrix multiplication, assuming a and b are affine: a * b

#### `makeRotate`

``` purescript
makeRotate :: Number -> Vec3N -> Mat4
```

Creates a transformation matrix for rotation in radians about the 3-element V.Vector axis.

#### `rotate`

``` purescript
rotate :: Number -> Vec3N -> Mat4 -> Mat4
```

Concatenates a rotation in radians about an axis to the given matrix.

#### `makeScale3`

``` purescript
makeScale3 :: Number -> Number -> Number -> Mat4
```

#### `makeScale`

``` purescript
makeScale :: Vec3N -> Mat4
```

#### `scale3`

``` purescript
scale3 :: Number -> Number -> Number -> Mat4 -> Mat4
```

Concatenates a scaling to the given matrix.

#### `scale`

``` purescript
scale :: Vec3N -> Mat4 -> Mat4
```

Concatenates a scaling to the given matrix.

#### `makeTranslate3`

``` purescript
makeTranslate3 :: Number -> Number -> Number -> Mat4
```

#### `makeTranslate`

``` purescript
makeTranslate :: Vec3N -> Mat4
```

#### `translate3`

``` purescript
translate3 :: Number -> Number -> Number -> Mat4 -> Mat4
```

Concatenates a translation to the given matrix.

#### `translate`

``` purescript
translate :: Vec3N -> Mat4 -> Mat4
```

Concatenates a translation to the given matrix.

#### `makeLookAt`

``` purescript
makeLookAt :: Vec3N -> Vec3N -> Vec3N -> Mat4
```

#### `makeBasis`

``` purescript
makeBasis :: Vec3N -> Vec3N -> Vec3N -> Mat4
```

Creates a transform from a basis consisting of 3 linearly independent V.Vectors.


## Module Data.ST.Matrix


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
freeze :: forall a h r. STArray h a -> Eff (st :: ST h | r) [a]
```

Create an immutable copy of a mutable array.

#### `thaw`

``` purescript
thaw :: forall a h r. [a] -> Eff (st :: ST h | r) (STArray h a)
```

Create a mutable copy of an immutable array.

#### `unsafeFreeze`

``` purescript
unsafeFreeze :: forall a h. STArray h a -> [a]
```

Freeze an ST array. Do not mutate the STArray afterwards!

#### `unsafeThaw`

``` purescript
unsafeThaw :: forall a h. [a] -> STArray h a
```


#### `cloneSTMat`

``` purescript
cloneSTMat :: forall s h a r. STMat s h a -> Eff (st :: ST h | r) (STMat s h a)
```


#### `identityST'`

``` purescript
identityST' :: forall s h r. (M.Matrix (M.Mat s) Number) => Eff (st :: ST h | r) (STMat s h Number)
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
fromMatrix :: forall s h r a. M.Mat s a -> Eff (st :: ST h | r) (STMat s h a)
```


#### `runSTMatrix`

``` purescript
runSTMatrix :: forall s a r. (forall h. Eff (st :: ST h | r) (STMat s h a)) -> Eff r (M.Mat s a)
```



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


## Module Test

#### `ble`

``` purescript
ble :: forall h r. Eff (st :: ST h | r) (M.STMat4 h)
```


#### `meh`

``` purescript
meh :: M.Vec3N
```


#### `ys`

``` purescript
ys :: M.Mat4
```


#### `zs`

``` purescript
zs :: M.Mat4
```