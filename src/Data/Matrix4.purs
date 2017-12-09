-----------------------------------------------------------------------------
--
-- Module      :  Matrix
-- Copyright   :  Jürgen Nicklisch-Franken
-- License     :  Apache-2.0
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
-- | Inspired by Mjs library for javascript
--
-----------------------------------------------------------------------------

module Data.Matrix4 where

import Prelude ((*), (+), (/), (==), (-), negate, ($))
import Data.TypeNat (Four)
import Data.Matrix (Mat(Mat), transpose, fromArrayColumns)
import Data.Vector3 (Vec3, cross) as V
import Data.Vector4 (Vec4) as V
import Data.Vector (Vec(Vec), normalize, direction, dot) as V
import Data.Maybe (Maybe(Just, Nothing))
import Math (sin, cos, sqrt, pi, tan)
import Extensions (fail)


type Vec3N = V.Vec3 Number
type Vec4N = V.Vec4 Number
type Mat4 = Mat Four Four Number

mat4 :: Array Number -> Mat4
mat4 = fromArrayColumns

identity :: Mat4
identity = Mat
           [1.0,0.0,0.0,0.0,
            0.0,1.0,0.0,0.0,
            0.0,0.0,1.0,0.0,
            0.0,0.0,0.0,1.0]

-- | Multiply a V.Vector by a 4x4 matrix: m * v
transform :: Mat4 -> Vec3N -> Vec3N
transform (Mat [x11, x12, x13, x14, x21, x22, x23, x24, x31, x32, x33, x34, x41, x42, x43, x44]) v =
  V.Vec [ (V.dot v t1 + x41) / w,
          (V.dot v t2 + x42) / w,
          (V.dot v t3 + x43) / w
        ]
  where
    t1 = V.Vec[x11,x21,x31]
    t2 = V.Vec[x12,x22,x32]
    t3 = V.Vec[x13,x23,x33]
    t4 = V.Vec[x14,x24,x34]
    w  = V.dot v t4 + x44

transform _ _ = fail "Matrix4>>transform: Impossible!"

-- | Computes the inverse of the given matrix m, assuming that the matrix is
--   orthonormal.
inverseOrthonormal :: Mat4 -> Mat4
inverseOrthonormal v@(Mat [x11, x12, x13, x14, x21, x22, x23, x24, x31, x32, x33, x34, x41, x42, x43, x44]) =
  case transpose v of
    Mat [y11, y12, y13, y14, y21, y22, y23, y24, y31, y32, y33, y34, y41, y42, y43, y44] ->
      let
        t = V.Vec [x41, x42, x43]
        r12 = negate (V.dot (V.Vec [y11,y21,y31]) t)
        r13 = negate (V.dot (V.Vec [y12,y22,y32]) t)
        r14 = negate (V.dot (V.Vec [y13,y23,y33]) t)
      in Mat [y11, y12, y13, 0.0, y21, y22, y23, 0.0, y31, y32, y33, 0.0, r12, r13, r14, y44]
    _ ->   fail "Matrix4>>inverseOrthonormal: Impossible!"
inverseOrthonormal _ = fail "Matrix4>>inverseOrthonormal: Impossible!"

--  Calculates the inverse matrix of a mat4
inverse :: Mat4 -> Maybe Mat4
inverse v@(Mat [a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, a22, a23, a30, a31, a32, a33]) =
    let b00 = a00 * a11 - a01 * a10
        b01 = a00 * a12 - a02 * a10
        b02 = a00 * a13 - a03 * a10
        b03 = a01 * a12 - a02 * a11
        b04 = a01 * a13 - a03 * a11
        b05 = a02 * a13 - a03 * a12
        b06 = a20 * a31 - a21 * a30
        b07 = a20 * a32 - a22 * a30
        b08 = a20 * a33 - a23 * a30
        b09 = a21 * a32 - a22 * a31
        b10 = a21 * a33 - a23 * a31
        b11 = a22 * a33 - a23 * a32

        d = (b00 * b11 - b01 * b10 + b02 * b09 + b03 * b08 - b04 * b07 + b05 * b06)
    in if d == 0.0
          then Nothing
          else let invDet = 1.0 / d
                 in Just $ Mat [(a11 * b11 - a12 * b10 + a13 * b09) * invDet,
                          (-a01 * b11 + a02 * b10 - a03 * b09) * invDet,
                          (a31 * b05 - a32 * b04 + a33 * b03) * invDet,
                          (-a21 * b05 + a22 * b04 - a23 * b03) * invDet,
                          (-a10 * b11 + a12 * b08 - a13 * b07) * invDet,
                          (a00 * b11 - a02 * b08 + a03 * b07) * invDet,
                          (-a30 * b05 + a32 * b02 - a33 * b01) * invDet,
                          (a20 * b05 - a22 * b02 + a23 * b01) * invDet,
                          (a10 * b10 - a11 * b08 + a13 * b06) * invDet,
                          (-a00 * b10 + a01 * b08 - a03 * b06) * invDet,
                          (a30 * b04 - a31 * b02 + a33 * b00) * invDet,
                          (-a20 * b04 + a21 * b02 - a23 * b00) * invDet,
                          (-a10 * b09 + a11 * b07 - a12 * b06) * invDet,
                          (a00 * b09 - a01 * b07 + a02 * b06) * invDet,
                          (-a30 * b03 + a31 * b01 - a32 * b00) * invDet,
                          (a20 * b03 - a21 * b01 + a22 * b00) * invDet]
inverse _ = fail "Matrix4>>inverse: Impossible!"

-- | Creates a matrix for a projection frustum with the given parameters.
-- Parameters:
-- * left - the left coordinate of the frustum
-- * right- the right coordinate of the frustum
-- * bottom - the bottom coordinate of the frustum
-- * top - the top coordinate of the frustum
-- * znear - the near z distance of the frustum
-- * zfar - the far z distance of the frustum

makeFrustum :: Number -> Number -> Number -> Number -> Number -> Number -> Mat4
makeFrustum left right bottom top znear zfar =
  Mat [ 2.0 * znear / (right-left), 0.0, 0.0, 0.0,
        0.0, 2.0 * znear / (top-bottom), 0.0, 0.0,
        (right+left) / (right-left), (top+bottom) / (top-bottom), -(zfar+znear) / (zfar-znear), (-1.0),
        0.0, 0.0, (-2.0) * zfar * znear / (zfar-znear), 0.0
      ]

-- | Creates a matrix for a perspective projection with the given parameters.
-- Parameters:
-- * fovy - field of view in the y axis, in degrees
-- * aspect - aspect ratio
-- * znear - the near z distance of the projection
-- * zfar - the far z distance of the projection
makePerspective :: Number -> Number -> Number -> Number -> Mat4
makePerspective fovy aspect znear zfar =
  makeFrustum xmin xmax ymin ymax znear zfar
  where
    ymax = znear * tan(fovy * pi / 360.0)
    ymin = -ymax
    xmin = ymin * aspect
    xmax = ymax * aspect


-- | Creates a matrix for an orthogonal frustum projection with the given parameters.
-- Parameters:
-- * left - the left coordinate of the frustum
-- * right- the right coordinate of the frustum
-- * bottom - the bottom coordinate of the frustum
-- * top - the top coordinate of the frustum
-- * znear - the near z distance of the frustum
-- * zfar - the far z distance of the frustum
makeOrtho :: Number -> Number -> Number -> Number -> Number -> Number -> Mat4
makeOrtho left right bottom top znear zfar =
  Mat [ x, 0.0, 0.0, 0.0,
        0.0, y, 0.0, 0.0,
        0.0, 0.0, z, 0.0,
        tX, tY, tZ, 1.0
      ]
  where
    tX = -(right+left)/(right-left)
    tY = -(top+bottom)/(top-bottom)
    tZ = -(zfar+znear)/(zfar-znear)
    x = 2.0 / (right-left)
    y = 2.0 / (top-bottom)
    z = -2.0 / (zfar-znear)



-- | Creates a matrix for a 2D orthogonal frustum projection with the given
-- parameters. `znear` and `zfar` are assumed to be -1 and 1, respectively.
-- Parameters:
-- * left - the left coordinate of the frustum
-- * right- the right coordinate of the frustum
-- * bottom - the bottom coordinate of the frustum
-- * top - the top coordinate of the frustum
makeOrtho2D :: Number -> Number -> Number -> Number -> Mat4
makeOrtho2D left right bottom top = makeOrtho left right bottom top (-1.0) 1.0

-- | Matrix multiplcation: a * b
mulM :: Mat4 -> Mat4 -> Mat4
mulM (Mat [x11, x21, x31, x41, x12, x22, x32, x42, x13, x23, x33, x43, x14, x24, x34, x44])
    (Mat [y11, y21, y31, y41, y12, y22, y32, y42, y13, y23, y33, y43, y14, y24, y34, y44]) =
     Mat [x11 * y11 + x12 * y21 + x13 * y31 + x14 * y41,
              x21 * y11 + x22 * y21 + x23 * y31 + x24 * y41,
                x31 * y11 + x32 * y21 + x33 * y31 + x34 * y41,
                  x41 * y11 + x42 * y21 + x43 * y31 + x44 * y41,
           x11 * y12 + x12 * y22 + x13 * y32 + x14 * y42,
              x21 * y12 + x22 * y22 + x23 * y32 + x24 * y42,
                x31 * y12 + x32 * y22 + x33 * y32 + x34 * y42,
                  x41 * y12 + x42 * y22 + x43 * y32 + x44 * y42,
            x11 * y13 + x12 * y23 + x13 * y33 + x14 * y43,
              x21 * y13 + x22 * y23 + x23 * y33 + x24 * y43,
                x31 * y13 + x32 * y23 + x33 * y33 + x34 * y43,
                  x41 * y13 + x42 * y23 + x43 * y33 + x44 * y43,
            x11 * y14 + x12 * y24 + x13 * y34 + x14 * y44,
              x21 * y14 + x22 * y24 + x23 * y34 + x24 * y44,
                x31 * y14 + x32 * y24 + x33 * y34 + x34 * y44,
                  x41 * y14 + x42 * y24 + x43 * y34 + x44 * y44]
mulM _ _ = fail "Matrix4>>mulM: Impossible!"

-- | Matrix multiplication, assuming a and b are affine: a * b
mulAffine :: Mat4 -> Mat4 -> Mat4
mulAffine (Mat [x11, x12, x13, x14, x21, x22, x23, x24, x31, x32, x33, x34, x41, x42, x43, x44])
          (Mat [y11, y12, y13, y14, y21, y22, y23, y24, y31, y32, y33, y34, y41, y42, y43, y44]) =
     Mat [x11 * y11 + x12 * y21 + x13 * y31,
              x21 * y11 + x22 * y21 + x23 * y31,
                x31 * y11 + x32 * y21 + x33 * y31,
                  0.0,
           x11 * y12 + x12 * y22 + x13 * y32,
              x21 * y12 + x22 * y22 + x23 * y32,
                x31 * y12 + x32 * y22 + x33 * y32,
                  0.0,
            x11 * y13 + x12 * y23 + x13 * y33,
              x21 * y13 + x22 * y23 + x23 * y33,
                x31 * y13 + x32 * y23 + x33 * y33,
                  0.0,
            x11 * y14 + x12 * y24 + x13 * y34 + x14,
              x21 * y14 + x22 * y24 + x23 * y34 + x24,
                x31 * y14 + x32 * y24 + x33 * y34 + x34,
                  1.0]
mulAffine _ _ = fail "Matrix4>>mulAffine: Impossible!"

-- | Creates a transformation matrix for rotation in radians about the 3-element V.Vector axis.
makeRotate :: Number -> Vec3N -> Mat4
makeRotate angle axis =
  case V.normalize axis of
    V.Vec [x,y,z] ->
      Mat [ x * x * c1 + c, y * x * c1 + z * s, z * x * c1 - y * s, 0.0,
            x * y * c1 - z * s, y * y * c1 + c, y * z * c1 + x * s, 0.0,
            x * z * c1 + y * s, y * z * c1 - x * s, z * z * c1 + c, 0.0,
            0.0, 0.0, 0.0, 1.0
          ]
    _ -> fail "Matrix4>>makeRotate: Impossible!"
  where
    c = cos angle
    c1 = 1.0-c
    s = sin angle

-- | Concatenates a rotation in radians about an axis to the given matrix.
rotate :: Number -> Vec3N -> Mat4 -> Mat4
rotate angle (V.Vec [a0, a1, a2]) (Mat [m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44]) =
  Mat [ m11 * t11 + m12 * t21 + m13 * t31,
        m21 * t11 + m22 * t21 + m23 * t31,
        m31 * t11 + m32 * t21 + m33 * t31,
        m41 * t11 + m42 * t21 + m43 * t31,

        m11 * t12 + m12 * t22 + m13 * t32,
        m21 * t12 + m22 * t22 + m23 * t32,
        m31 * t12 + m32 * t22 + m33 * t32,
        m41 * t12 + m42 * t22 + m43 * t32,

        m11 * t13 + m12 * t23 + m13 * t33,
        m21 * t13 + m22 * t23 + m23 * t33,
        m31 * t13 + m32 * t23 + m33 * t33,
        m41 * t13 + m42 * t23 + m43 * t33,

        m14, m24, m34, m44
      ]
  where
    l = sqrt (a0*a0 + a1*a1 + a2*a2)
    im = 1.0 / l
    x = a0 * im
    y = a1 * im
    z = a2 * im
    c = cos angle
    c1 = 1.0-c
    s = sin angle
    xs = x*s
    ys = y*s
    zs = z*s
    xyc1 = x * y * c1
    xzc1 = x * z * c1
    yzc1 = y * z * c1
    t11 = x * x * c1 + c
    t21 = xyc1 + zs
    t31 = xzc1 - ys
    t12 = xyc1 - zs
    t22 = y * y * c1 + c
    t32 = yzc1 + xs
    t13 = xzc1 + ys
    t23 = yzc1 - xs
    t33 = z * z * c1 + c

rotate _ _ _ = fail "Matrix4>>rotate: Impossible!"

-- | Creates a transformation matrix for scaling by 3 scalar values, one for
-- each of the x, y, and z directions.
makeScale3 :: Number -> Number -> Number -> Mat4
makeScale3 x y z = Mat [x,0.0,0.0,0.0,
                        0.0,y,0.0,0.0,
                        0.0,0.0,z,0.0,
                        0.0,0.0,0.0,1.0]

-- | Creates a transformation matrix for scaling each of the x, y, and z axes by
-- the amount given in the corresponding element of the 3-element V.Vector.
makeScale :: Vec3N -> Mat4
makeScale (V.Vec [x,y,z]) = makeScale3 x y z
makeScale _ = fail "Matrix4>>makeScale: Impossible!"

-- | Concatenates a scaling to the given matrix.
scale3 :: Number -> Number -> Number -> Mat4 -> Mat4
scale3 x y z (Mat [x11, x12, x13, x14, x21, x22, x23, x24, x31, x32, x33, x34, x41, x42, x43, x44]) =
  Mat [x11*x, x12*x, x13*x, x14*x,
        x21*y, x22*y, x23*y, x24*y,
        x31*z, x32*z, x33*z, x34*z,
        x41, x42, x43, x44]
scale3 _ _ _ _ = fail "Matrix4>>scale3: Impossible!"

-- | Concatenates a scaling to the given matrix.
scale :: Vec3N -> Mat4 -> Mat4
scale (V.Vec [x,y,z]) = scale3 x y z
scale _ = fail "Matrix4>>scale: Impossible!"

-- | Creates a transformation matrix for translating by 3 scalar values, one for
-- each of the x, y, and z directions.
makeTranslate3 :: Number -> Number -> Number -> Mat4
makeTranslate3 x y z = Mat [1.0,0.0,0.0,0.0,
                            0.0,1.0,0.0,0.0,
                            0.0,0.0,1.0,0.0,
                            x,y,z,1.0]

-- | Creates a transformation matrix for translating each of the x, y, and z
-- axes by the amount given in the corresponding element of the 3-element V.Vector.
makeTranslate :: Vec3N -> Mat4
makeTranslate (V.Vec [x,y,z]) = makeTranslate3 x y z
makeTranslate _ = fail "Matrix4>>makeTranslate: Impossible!"

-- | Concatenates a translation to the given matrix.
translate3 :: Number -> Number -> Number -> Mat4 -> Mat4
translate3 x y z (Mat [m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44]) =
  Mat [m11, m21, m31, m41,
        m12, m22, m32, m42,
        m13, m23, m33, m43,
        m11 * x + m12 * y + m13 * z + m14,
          m21 * x + m22 * y + m23 * z + m24,
            m31 * x + m32 * y + m33 * z + m34,
              m41 * x + m42 * y + m43 * z + m44]
translate3 x y z _ = fail "Matrix3>>translate3: Impossible!"

-- | Concatenates a translation to the given matrix.
translate :: Vec3N -> Mat4 -> Mat4
translate (V.Vec [x,y,z]) m = translate3 x y z m
translate _ _               = fail "Matrix3>>translate: Impossible!"

-- | Creates a transformation matrix for a camera.
-- Parameters:
--  * eye - The location of the camera
--  * center - The location of the focused object
--  * up - The "up" direction according to the camera
makeLookAt :: Vec3N -> Vec3N -> Vec3N -> Mat4
makeLookAt eye@(V.Vec [e0,e1,e2]) center up =
  case V.direction eye center of
    z@V.Vec [z0,z1,z2] ->
      case V.normalize (V.cross up z) of
        x@V.Vec [x0,x1,x2] ->
          case V.normalize (V.cross z x) of
            y@V.Vec [y0,y1,y2] ->
              let m1 = Mat [x0,y0,z0,0.0,
                             x1,y1,z1,0.0,
                             x2,y2,z2,0.0,
                             0.0,0.0,0.0,1.0]
                  m2 = Mat [1.0,0.0,0.0,0.0,
                            0.0,1.0,0.0,0.0,
                            0.0,0.0,1.0,0.0,
                            (-e0),(-e1),(-e2),1.0]
                in mulM m1 m2
            _ -> fail "Matrix4>>makeRotate: Impossible!"
        _ -> fail "Matrix4>>makeRotate: Impossible!"
    _ -> fail "Matrix4>>makeRotate: Impossible!"
makeLookAt _ _ _ = fail "Matrix4>>makeLookAt: Impossible!"

-- | Creates a transform from a basis consisting of 3 linearly independent V.Vectors.
makeBasis :: Vec3N -> Vec3N -> Vec3N -> Mat4
makeBasis (V.Vec [x0,x1,x2]) (V.Vec [y0,y1,y2]) (V.Vec [z0,z1,z2])=
  Mat [x0,x1,x2,0.0,
        y0,y1,y2,0.0,
        z0,z1,z2,0.0,
        0.0,0.0,0.0,1.0]
makeBasis _ _ _ = fail "Matrix4>>makeBasis: Impossible!"

project :: Vec3N
    -> Mat4 -- modelview matrix
     -> Mat4 -- projection matrix
     -> Vec4N -- viewport
     -> Maybe Vec3N
project (V.Vec [objx,objy,objz])
        (Mat [m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44])
        (Mat [p11, p21, p31, p41, p12, p22, p32, p42, p13, p23, p33, p43, p14, p24, p34, p44])
        (V.Vec [vp0,vp1,vp2,vp3]) =
    let temp1 = [m11 * objx + m12 * objy + m13 * objz + m14,  -- w is always 1
                m21 * objx + m22 * objy + m23 * objz + m24,
                m31 * objx + m32 * objy + m33 * objz + m34,
                m41 * objx + m42 * objy + m43 * objz + m44]
        -- Projection transform, the final row of projection matrix is always [0 0 -1 0]
        -- so we optimize for that.
    in case temp1 of
        [ft0,ft1,ft2,ft3] ->
            let temp2 = [p11 * ft0 + p12 * ft1 + p13 * ft2 + p14 * ft3,
                         p21 * ft0 + p22 * ft1 + p23 * ft2 + p24 * ft3,
                         p31 * ft0 + p32 * ft1 + p33 * ft2 + p34 * ft3,
                         -ft2]
            in case temp2 of
                [gt0,gt1,gt2,gt3] ->
                    if gt3 == 0.0
                        then Nothing --The w value
                        else
                            let gt3' = 1.0 / gt3
                            --Perspective division
                                gt0' = gt0 * gt3'
                                gt1' = gt1 * gt3'
                                gt2' = gt2 * gt3'
                            -- Window coordinates
                            -- Map x, y to range 0-1
                            in Just (V.Vec [(gt0' * 0.5 + 0.5) * vp2 + vp0,
                                           (gt1' * 0.5 + 0.5) * vp3 + vp1,
                                           -- This is only correct when glDepthRange(0.0, 1.0)
                                           (1.0 + gt2') * 0.5])	-- Between 0 and 1
                _ -> fail "Matrix4>>project: Impossible!"
        _ ->    fail "Matrix4>>project: Impossible!"
project _ _ _ _ = fail "Matrix4>>project: Impossible!"

unProject :: Vec3N
    -> Mat4 -- modelview matrix
    -> Mat4 -- projection matrix
    -> Vec4N -- viewport
    -> Maybe Vec3N
unProject (V.Vec [winx,winy,winz])
        modelView
        projection
        (V.Vec [vp0,vp1,vp2,vp3]) =
    let mvProjMatrix = mulM projection modelView
    in case inverse mvProjMatrix of
        Nothing -> Nothing
        Just inv -> let inVect = V.Vec [(winx - vp0) / vp2 * 2.0 - 1.0,
                                      (winy - vp1) / vp3 * 2.0 - 1.0,
                                      2.0 * winz - 1.0,
                                      1.0]
                        out = mulMatVect inv inVect
                    in case out of
                        V.Vec [out0, out1, out2, out3] ->
                            if out3 == 0.0
                                then Nothing
                                else let out3' = 1.0 / out3
                                     in Just (V.Vec [out0 * out3',out1 * out3',out2 * out3'])
                        _ -> fail "Matrix4>>unProject: Impossible!"
unProject _ _ _ _ = fail "Matrix4>>unProject: Impossible!"

mulMatVect :: Mat4 -> Vec4N -> Vec4N
mulMatVect
    (Mat [m11, m21, m31, m41, m12, m22, m32, m42, m13, m23, m33, m43, m14, m24, m34, m44])
    (V.Vec [v0,v1,v2,v3])
    = V.Vec [m11 * v0 + m12 * v1 + m13 * v2 + m14 * v3,
            m21 * v0 + m22 * v1 + m23 * v2 + m24 * v3,
            m31 * v0 + m32 * v1 + m33 * v2 + m34 * v3,
            m41 * v0 + m42 * v1 + m43 * v2 + m44 * v3]
mulMatVect _ _ = fail "Matrix4>>mulMatVect: Impossible!"
