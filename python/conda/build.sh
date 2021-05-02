#!/bin/bash

set -ex

project_dir=/home/tom/src/ising-ground-state/
install -v -m 644 "$project_dir/prefix/include/ising_ground_state.h" "$PREFIX/include"
install -v -m 644 "$project_dir/prefix/lib/pkgconfig/ising_ground_state.pc" "$PREFIX/lib/pkgconfig"
install -v -m 644 "$project_dir/prefix/lib/libising_ground_state.so" "$PREFIX/lib/"

cd $PREFIX/lib
ls -l libising*

cd $PREFIX/include
ls -l ising*

# find "prefix/lib" -name "*ising_ground_state.so*" -maxdepth 1 -type f | while read -r sofile; do
#   # shellcheck disable=SC2016
#   echo "Setting rpath of $sofile to" '$ORIGIN'
#   # shellcheck disable=SC2016
#   patchelf --set-rpath "$PREFIX" --force-rpath "$sofile"
#   patchelf --print-rpath "$sofile"
# done

# $PYTHON setup.py install
