# Module Documentation

## Module Data.Array.Extended

### Values

    slice :: forall a. Number -> Number -> [a] -> [a]


## Module Data.Matrix

### Types

    newtype Mat s a where
      Mat :: [a] -> Mat s a


### Type Classes

    class (Sized (m a)) <= Matrix m a where
      generate :: (Number -> Number -> a) -> m a


### Type Class Instances

    instance applyMat :: Apply (Mat s)

    instance eqMat :: (Eq a) => Eq (Mat s a)

    instance functorMat :: Functor (Mat s)

    instance m2 :: Matrix (Mat Two) a

    instance m3 :: Matrix (Mat Three) a

    instance m4 :: Matrix (Mat Four) a

    instance showMat2 :: (Show a) => Show (Mat Two a)

    instance showMat3 :: (Show a) => Show (Mat Three a)

    instance showMat4 :: (Show a) => Show (Mat Four a)

    instance sm2 :: Sized (Mat Two a)

    instance sm3 :: Sized (Mat Three a)

    instance sm4 :: Sized (Mat Four a)


### Values

    columns :: forall s a. (Matrix (Mat s) a) => Mat s a -> [[a]]

    fromArray :: forall a s. (Matrix (Mat s) a) => [a] -> Mat s a

    generate_ :: forall a s. Number -> (Number -> Number -> a) -> Mat s a

    getElem :: forall s a. (Matrix (Mat s) a) => Number -> Number -> Mat s a -> a

    identity :: forall s a. (Matrix (Mat s) Number) => Mat s Number

    scaleMatrix :: forall a s. (Matrix (Mat s) a, Num a) => a -> Mat s a -> Mat s a

    toArray :: forall s a. Mat s a -> [a]

    transpose :: forall a s. (Matrix (Mat s) a) => Mat s a -> Mat s a


## Module Data.Matrix3

### Types

    type Mat3 = Mat Three Number


### Values

    mat3 :: [Number] -> Mat3

    normalFromMat4 :: Mat Four Number -> Maybe Mat3


## Module Data.Matrix4

### Types

    type Mat4 = Mat Four Number

    type Vec3N = V3.Vec3 Number


### Values

    inverseOrthonormal :: Mat4 -> Mat4

    makeBasis :: Vec3N -> Vec3N -> Vec3N -> Mat4

    makeFrustum :: Number -> Number -> Number -> Number -> Number -> Number -> Mat4

    makeLookAt :: Vec3N -> Vec3N -> Vec3N -> Mat4

    makeOrtho :: Number -> Number -> Number -> Number -> Number -> Number -> Mat4

    makeOrtho2D :: Number -> Number -> Number -> Number -> Mat4

    makePerspective :: Number -> Number -> Number -> Number -> Mat4

    makeRotate :: Number -> Vec3N -> Mat4

    makeScale :: Vec3N -> Mat4

    makeScale3 :: Number -> Number -> Number -> Mat4

    makeTranslate :: Vec3N -> Mat4

    makeTranslate3 :: Number -> Number -> Number -> Mat4

    mat4 :: [Number] -> Mat4

    mul :: Mat4 -> Mat4 -> Mat4

    mulAffine :: Mat4 -> Mat4 -> Mat4

    rotate :: Number -> Vec3N -> Mat4 -> Mat4

    scale :: Vec3N -> Mat4 -> Mat4

    scale3 :: Number -> Number -> Number -> Mat4 -> Mat4

    transform :: Mat4 -> Vec3N -> Vec3N

    translate :: Vec3N -> Mat4 -> Mat4

    translate3 :: Number -> Number -> Number -> Mat4 -> Mat4