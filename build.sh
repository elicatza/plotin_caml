#!/usr/bin/env bash

set -xe

CFLAGS="-Wall -Wextra -pedantic"
LIBS="-I$(ocamlc -where) $(pkg-config --libs raylib)"

clang -shared -fPIC $CFLAGS ./caml_raylib.c -o dllcaml_raylib.so $LIBS

ocamlc -c ./raylib.ml
ocamlc -c ./plot.ml 
ocamlc raylib.cmo plot.cmo ./main.ml -o plot -dllpath . -dllib -lcaml_raylib
