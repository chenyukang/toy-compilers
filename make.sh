#!/bin/bash

ocamlfind ocamlc -package js_of_ocaml -thread -syntax camlp4o -package js_of_ocaml.syntax -linkpkg -o brain.byte brain.ml
js_of_ocaml brain.byte -o sites/brain.js


ocamlfind ocamlc -package js_of_ocaml -thread -syntax camlp4o -package js_of_ocaml.syntax -linkpkg -o while.byte while.ml
js_of_ocaml while.byte -o sites/while.js
