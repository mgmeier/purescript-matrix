/* global exports */

// module Data.ST.Matrix


    "use strict";

    exports.copyImpl = function(arr) {
        return function(){
            var as = arr.slice();
            return as;
        };
    };

    exports.unsafeFreeze = function(arr) {
        return arr;
    };

    exports.unsafeThaw = function(arr) {
        return arr;
    };

    exports.copyToSTMat = function(m) {
        return function(mst){
            return function(){
                for (var i=0; i<m.length; i++){
                    mst[i] = m[i];
                }
                return {};
            };
        };
    };

    exports.scaleSTMatrixInt = function(__dict_Num) {
      return function(x){
        return function(arr){
           return function(){
              var l=arr.length;
              for (var i=0; i<l; i++){
                arr[i] *= x;
              }
           };
        };
      };
    };

    exports.runSTMatrix = function (f) {
        return (f);
    };
