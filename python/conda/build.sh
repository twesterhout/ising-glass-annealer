#!/bin/bash

set -ex

install -v -m 644 "prefix/include/ising_glass_annealer.h" "$PREFIX/include"
install -v -m 644 "prefix/lib/pkgconfig/ising_glass_annealer.pc" "$PREFIX/lib/pkgconfig"
install -v -m 644 "prefix/lib/libising_glass_annealer.so" "$PREFIX/lib/"

sofile="$PREFIX/lib/libising_glass_annealer.so"
patchelf --set-rpath "$PREFIX/$HOST/sysroot/lib" --force-rpath "$sofile"

$PYTHON setup.py install
