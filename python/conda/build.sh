#!/bin/bash

set -ex

install -v -m 644 "prefix/include/ising_ground_state.h" "$PREFIX/include"
install -v -m 644 "prefix/lib/pkgconfig/ising_ground_state.pc" "$PREFIX/lib/pkgconfig"
install -v -m 644 "prefix/lib/libising_ground_state.so" "$PREFIX/lib/"

find "$PREFIX/lib" -name "*ising_ground_state.so*" -maxdepth 1 -type f | while read -r sofile; do
  # shellcheck disable=SC2016
  # echo "Setting rpath of $sofile to" '$ORIGIN'
  # shellcheck disable=SC2016
  patchelf --set-rpath "$PREFIX/$HOST/sysroot/lib" --force-rpath "$sofile"
  # patchelf --replace-needed "/lib64/ld-linux-x86-64.so.2" "$PREFIX/$HOST/sysroot/lib/ld-linux-x86-64.so.2" "$sofile"
  # ldd "$sofile"
done

$PYTHON setup.py install
