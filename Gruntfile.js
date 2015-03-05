module.exports = function(grunt) {

    "use strict";

    grunt.initConfig({ 

        libFiles: [
          "src/**/*.purs",
          "bower_components/purescript-*/src/**/*.purs",
        ],

        clean: ["output"],
        
        psc: {
            options: {
                main: "Data.ST.Matrix",
                modules: ["Data.ST.Matrix"]
            },
            test1: {
              src: ["src/Data/ST/Matrix.purs","<%=libFiles%>"],
              dest: "test1.js"
            }
        },

        pscMake: ["<%=libFiles%>"],
        dotPsci: ["<%=libFiles%>"],
        pscDocs: {
            readme: {
                src: "src/**/*.purs",
                dest: "docs/Module.md"
            }
        }

    });

    grunt.loadNpmTasks("grunt-contrib-clean");
    grunt.loadNpmTasks("grunt-purescript");

    grunt.registerTask("make", ["pscMake", "dotPsci", "pscDocs"]);
    grunt.registerTask("default", ["make"]);
    grunt.registerTask("test1", ["psc:test1"]);
};
