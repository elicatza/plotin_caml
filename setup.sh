#!/usr/bin/env bash

set -xe

mkdir -p vendor

# Ubuntu dependencies: sudo apt install libasound2-dev libx11-dev libxrandr-dev libxi-dev libgl1-mesa-dev libglu1-mesa-dev libxcursor-dev libxinerama-dev

# Add Raylib to ./vendor and install to ./lib/raylib
# git clone --depth 1 "https://github.com/raysan5/raylib" vendor/raylib
mkdir -p lib/raylib
mkdir -p vendor/raylib/build && cd vendor/raylib/build 
cmake -DCMAKE_INSTALL_PREFIX=../../../lib/raylib .. -DWITH_PIC=ON
make -j5
make install
