#!/usr/bin/env bash

set -eu

PREFIX="$PWD/python/prefix"
LIBRARY_NAME="ising_glass_annealer"
declare -a HS_LD_FLAGS
declare -a C_LD_FLAGS

cabal_build_dir() {
  declare -r ghc_version=$(ghc --version | tr ' ' '\n' | tail -n 1)
  realpath "$(dirname $(find dist-newstyle -name "init.o" | grep "$ghc_version"))/.."
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
  # set -x
  # -optl -Wl,--retain-symbols-file=$PWD/api.txt
  ghc --make -v -no-hs-main -pgmc "$CC" -pgml "$CC" -shared "$(cabal_build_dir)/cbits/init.o" \
    -o "$PREFIX/lib/lib${LIBRARY_NAME}.so" "${HS_LD_FLAGS[@]}" "${C_LD_FLAGS[@]}"
  # set +x
}

create_static_library() {
  # --retain-symbols=$PWD/api.txt
  ld -o "$PREFIX/lib/lib${LIBRARY_NAME}.a" --relocatable --whole-archive "${HS_LD_FLAGS[@]}"
}

create_pkgconfig_file() {
  cat >"${PREFIX}/lib/pkgconfig/${LIBRARY_NAME}.pc" <<EOF
prefix=${PREFIX}
libdir=${PREFIX}/lib
includedir=${PREFIX}/include

Name: Dummy
Description: Dummy
Version: 0.1.0.0
Libs: -L\${libdir} -l${LIBRARY_NAME}
Libs.private:
Cflags: -I\${includedir}
EOF
}

main() {
  init_flags
  mkdir -p "$PREFIX/include"
  mkdir -p "$PREFIX/lib/pkgconfig"
  create_shared_library
  create_static_library
  create_pkgconfig_file
  install -m 644 "cbits/${LIBRARY_NAME}.h" "$PREFIX/include"
}

main
