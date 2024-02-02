#!/usr/bin/env bash

set -xe

export PKG_CONFIG_PATH=lib/raylib/lib/pkgconfig/
CFLAGS="-Wall -Wextra -pedantic"
LIBS="-I$(ocamlc -where) $(pkg-config --libs raylib --cflags raylib) -lm -lglfw -lpthread"

clang -shared -fPIC $CFLAGS ./caml_raylib.c -o dllcaml_raylib.so $LIBS

ocamlc -c ./raylib.ml
ocamlc -c ./plot.ml 
ocamlc raylib.cmo plot.cmo ./main.ml -o plot -dllpath . -dllib -lcaml_raylib
