## Module Data.Matrix4

#### `Vec3N`

``` purescript
type Vec3N = Vec3 Number
```

#### `Vec4N`

``` purescript
type Vec4N = Vec4 Number
```

#### `Mat4`

``` purescript
type Mat4 = Mat Four Number
```

#### `mat4`

``` purescript
mat4 :: Array Number -> Mat4
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

#### `mulM`

``` purescript
mulM :: Mat4 -> Mat4 -> Mat4
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

#### `project`

``` purescript
project :: Vec3N -> Mat4 -> Mat4 -> Vec4N -> Maybe Vec3N
```

#### `unProject`

``` purescript
unProject :: Vec3N -> Mat4 -> Mat4 -> Vec4N -> Maybe Vec3N
```

#### `mulMatVect`

``` purescript
mulMatVect :: Mat4 -> Vec4N -> Vec4N
```


