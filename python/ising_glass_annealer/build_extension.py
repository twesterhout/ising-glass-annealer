from cffi import FFI
import glob
import os


def get_declarations():
    with open("ising_glass_annealer/ising_glass_annealer_declarations.h", "r") as f:
        return f.read()


ffibuilder = FFI()

ffibuilder.cdef(get_declarations())

ffibuilder.set_source(
    "ising_glass_annealer._ising_glass_annealer_hs",
    """
#include "ising_glass_annealer.h"
#include <Python.h>
""",
    extra_compile_args=["-Wall", "-Wextra"],
    libraries=["ising_glass_annealer"],
)

if __name__ == "__main__":
    ffibuilder.compile(verbose=True)
