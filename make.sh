#!/bin/bash

rm -rf *.cmi
rm -rf *.cmo
rm -rf *.native
rm -rf *.byte
rm -rf sites/*.js

COMP="ocamlfind ocamlc -package js_of_ocaml -thread -syntax camlp4o -package js_of_ocaml.syntax"

`$COMP -c interp.mli`
`$COMP -c load.ml`

`$COMP -c brain.ml`
`$COMP -c brain_top.ml`
`$COMP -linkpkg brain.cmo load.cmo brain_top.cmo -o brain.byte`
js_of_ocaml brain.byte -o sites/brain.js


`$COMP -c while.ml`
`$COMP -c while_top.ml`
`$COMP -linkpkg while.cmo load.cmo while_top.cmo -o while.byte`
js_of_ocaml while.byte -o sites/while.js


`$COMP -c while_func.ml`
`$COMP -c while_func_top.ml`
`$COMP -linkpkg while_func.cmo load.cmo while_func_top.cmo -o while_func.byte`
js_of_ocaml while_func.byte -o sites/while_func.js


`$COMP -c while_func_demo.ml`
`$COMP -linkpkg while_func.cmo while_func_demo.cmo -o while_func_demo.byte`
