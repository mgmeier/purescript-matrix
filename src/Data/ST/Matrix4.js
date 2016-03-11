/* global exports */

// module Data.ST.Matrix4


    "use strict";

    exports.identityST = function() {
        var as = [1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1];
        return as;
    };

    exports.rotateST = function(angle) {
      return function(a){
        return function(arr){
           return function(){
             var l      = Math.sqrt (a[0]*a[0] + a[1]*a[1] + a[2]*a[2]);
             var im     = 1.0 / l;
             var x      = a[0] * im;
             var y      = a[1] * im;
             var z      = a[2] * im;
             var c      = Math.cos (angle);
             var c1     = 1 - c;
             var s      = Math.sin (angle);
             var xs     = x*s;
             var ys     = y*s;
             var zs     = z*s;
             var xyc1   = x*y*c1;
             var xzc1   = x*z*c1;
             var yzc1   = y*z*c1;
             var t11    = x*x*c1+c;
             var t21    = xyc1+zs;
             var t31    = xzc1-ys;
             var t12    = xyc1-zs;
             var t22    = y*y*c1+c;
             var t32    = yzc1+xs;
             var t13    = xzc1+ys;
             var t23    = yzc1-xs;
             var t33    = z*z*c1+c;

             var m  = arr.slice();
             for (var i=0; i<4; i++){
                arr[i] = m[i] * t11 + m[i+4] * t21 + m[i+8] * t31;
                arr[i+4] = m[i] * t12 + m[i+4] * t22 + m[i+8] * t32;
                arr[i+8] = m[i] * t13 + m[i+4] * t23 + m[i+8] * t33;
             }
           };
        };
      };
    };

    exports.rotateSTX = function(angle) {
        return function(arr){
           return function(){
             var cosAngle    = Math.cos (angle);
             var sinAngle    = Math.sin (angle);
             var t1 = arr[4] * cosAngle + arr[8] * sinAngle;
             var t2 = arr[5] * cosAngle + arr[9] * sinAngle;
             var t3 = arr[6] * cosAngle + arr[10] * sinAngle;
             var t4 = arr[7] * cosAngle + arr[11] * sinAngle;
             var t5 = arr[4] * -sinAngle + arr[8] * cosAngle;
             var t6 = arr[5] * -sinAngle + arr[9] * cosAngle;
             var t7 = arr[6] * -sinAngle + arr[10] * cosAngle;
             var t8 = arr[7] * -sinAngle + arr[11] * cosAngle;
             arr[4] = t1;
             arr[5] = t2;
             arr[6] = t3;
             arr[7] = t4;
             arr[8] = t5;
             arr[9] = t6;
             arr[10] = t7;
             arr[11] = t8;
         };
        };
    };

    exports.rotateSTY = function(angle) {
        return function(arr){
           return function(){
             var cosAngle    = Math.cos (angle);
             var sinAngle    = Math.sin (angle);
             var t1 = arr[0] * cosAngle + arr[8] * -sinAngle;
             var t2 = arr[1] * cosAngle + arr[9] * -sinAngle;
             var t3 = arr[2] * cosAngle + arr[10] * -sinAngle;
             var t4 = arr[3] * cosAngle + arr[11] * -sinAngle;
             var t5 = arr[0] * sinAngle + arr[8] * cosAngle;
             var t6 = arr[1] * sinAngle + arr[9] * cosAngle;
             var t7 = arr[2] * sinAngle + arr[10] * cosAngle;
             var t8 = arr[3] * sinAngle + arr[11] * cosAngle;
             arr[0] = t1;
             arr[1] = t2;
             arr[2] = t3;
             arr[3] = t4;
             arr[8] = t5;
             arr[9] = t6;
             arr[10] = t7;
             arr[11] = t8;
           };
        };
    };

    exports.rotateSTZ = function(angle) {
        return function(arr){
           return function(){
             var cosAngle    = Math.cos (angle);
             var sinAngle    = Math.sin (angle);
             var t1 = arr[0] * cosAngle + arr[4] * sinAngle;
             var t2 = arr[1] * cosAngle + arr[5] * sinAngle;
             var t3 = arr[2] * cosAngle + arr[6] * sinAngle;
             var t4 = arr[3] * cosAngle + arr[7] * sinAngle;
             var t5 = arr[0] * -sinAngle + arr[4] * cosAngle;
             var t6 = arr[1] * -sinAngle + arr[5] * cosAngle;
             var t7 = arr[2] * -sinAngle + arr[6] * cosAngle;
             var t8 = arr[3] * -sinAngle + arr[7] * cosAngle;
             arr[0] = t1;
             arr[1] = t2;
             arr[2] = t3;
             arr[3] = t4;
             arr[4] = t5;
             arr[5] = t6;
             arr[6] = t7;
             arr[7] = t8;
         };
        };
    };


    exports.translateST = function(a) {
      return function(m){
           return function(){
             for (var i=0; i<4; i++){
                m[i+12] += m[i] * a[0] + m[i+4] * a[1] + m[i+8] * a[2];
             }
           };
      };
    };

    exports.scaleST3 = function(x) {
        return function(y){
            return function(z){
                return function(m){
                    return function(){
                        m[0] = m[0]*x;
                        m[1] = m[1]*x;
                        m[2] = m[2]*x;
                        m[3] = m[3]*x;

                        m[4] = m[4]*y;
                        m[5] = m[5]*y;
                        m[6] = m[6]*y;
                        m[7] = m[7]*y;

                        m[8] = m[8]*z;
                        m[9] = m[9]*z;
                        m[10] = m[10]*z;
                        m[11] = m[11]*z;

                        };
                    };
                };
            };
        };
