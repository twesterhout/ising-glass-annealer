#!/usr/bin/env bash

set -eu

PREFIX="$PWD/prefix"
declare -a HS_LD_FLAGS
declare -a C_LD_FLAGS

cabal_build_dir() {
  realpath "$(dirname $(find dist-newstyle -name "init.o"))/.."
}

init_flags() {
  declare -r AUTOGEN_DIR="$(cabal_build_dir)/global-autogen"
  while IFS= read -r path; do
    HS_LD_FLAGS+=("-L$path")
  done <"$AUTOGEN_DIR/HS_LIBRARY_PATHS_LIST"
  HS_LD_FLAGS+=("-L$(cabal_build_dir)")

  while IFS= read -r lib; do
    HS_LD_FLAGS+=("-l$lib")
  done <"$AUTOGEN_DIR/HS_LIBRARIES_LIST"

  while IFS= read -r lib; do
    C_LD_FLAGS+=("-l$lib")
  done <"$AUTOGEN_DIR/EXTRA_LIBRARIES_LIST"
}

create_shared_library() {
  set -x
  # -optl -Wl,--retain-symbols-file=$PWD/api.txt
  ghc --make -v -no-hs-main -shared "$(cabal_build_dir)/cbits/init.o" \
    -o "$PREFIX/lib/libising_ground_state.so" "${HS_LD_FLAGS[@]}" "${C_LD_FLAGS[@]}"
  set +x
}

create_static_library() {
  # --retain-symbols=$PWD/api.txt
  ld -o "$PREFIX/lib/libbar.a" --relocatable --whole-archive "${HS_LD_FLAGS[@]}"
}

main() {
  init_flags
  mkdir -p "$PREFIX/include"
  mkdir -p "$PREFIX/lib"
  create_shared_library
  create_static_library
  install -m 644 "cbits/ising_ground_state.h" "$PREFIX/include"
}

main
