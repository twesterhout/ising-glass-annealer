# Copyright (c) 2021, Tom Westerhout
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

__version__ = "0.4.0.1"
__author__ = "Tom Westerhout <14264576+twesterhout@users.noreply.github.com>"

import os
import sys
import time
import weakref
from typing import Any, Optional

import cffi
import numpy as np
import scipy.sparse
from numpy.typing import NDArray

# Enable import warnings
# warnings.filterwarnings("default", category=ImportWarning)


def get_library_name() -> str:
    if sys.platform == "linux":
        extension = ".so"
    elif sys.platform == "darwin":
        extension = ".dylib"
    else:
        raise ImportError("Unsupported platform: {}".format(sys.platform))
    return "libising_glass_annealer{}".format(extension)


def get_package_path() -> str:
    """Get current package installation path."""
    return os.path.dirname(os.path.realpath(__file__))


ffi = cffi.FFI()


def load_shared_library():
    ffi.cdef(
        """
        void sa_init(void);
        void sa_exit(void);
        void sa_anneal_f64(int32_t,
                           double const *, int32_t const *, int32_t const *,
                           double const *,
                           uint32_t,
                           int32_t,
                           int32_t,
                           float, float,
                           uint64_t *,
                           double *);
        double sa_compute_energy_f64(int32_t,
                                     double const *, int32_t const *, int32_t const *,
                                     double const *,
                                     uint64_t const *);
        void sa_estimate_betas_f64(int32_t,
                                   double const *, int32_t const *, int32_t const *,
                                   double const *,
                                   double *, double *);
    """
    )
    return ffi.dlopen(os.path.join(get_package_path(), get_library_name()))


_lib = load_shared_library()
_lib.sa_init()
_finalizer = weakref.finalize(_lib, _lib.sa_exit)


class Hamiltonian:
    exchange: scipy.sparse.csr_matrix
    field: NDArray[np.float64]

    def __init__(self, exchange: scipy.sparse.spmatrix, field: NDArray[np.float64]):
        self.exchange = exchange.tocsr()
        self.field = np.asarray(field, dtype=np.float64)

    @property
    def size(self) -> int:
        (n, m) = self.exchange.shape
        assert n == m
        return n

    def energy(self, x: NDArray[np.uint64]) -> float:
        n_bits = self.size
        n_words = (n_bits + 63) // 64
        x = np.ascontiguousarray(x, dtype=np.uint64)
        if x.shape != (n_words,):
            raise ValueError("'x' has wrong shape: {}; expected {}".format(x.shape, (n_words,)))
        if not isinstance(self.exchange, scipy.sparse.csr_matrix):
            self.exchange = self.exchange.tocsr()
        elts = np.asarray(self.exchange.data, dtype=np.float64)
        cols = np.asarray(self.exchange.indices, dtype=np.int32)
        rows = np.asarray(self.exchange.indptr, dtype=np.int32)
        field = np.asarray(self.field, dtype=np.float64)
        e = _lib.sa_compute_energy_f64(
            n_bits,
            ffi.from_buffer("double const[]", elts, require_writable=False),
            ffi.from_buffer("int32_t const[]", cols, require_writable=False),
            ffi.from_buffer("int32_t const[]", rows, require_writable=False),
            ffi.from_buffer("double const[]", field, require_writable=False),
            ffi.from_buffer("uint64_t const[]", x, require_writable=False),
        )
        return float(e)


def signs_to_bits(signs: NDArray[Any]) -> NDArray[np.uint64]:
    signs = np.sign(signs)
    mask = signs == 1
    assert np.all(mask | (signs == -1))
    bits = np.packbits(mask, axis=-1, bitorder="little")
    rem = len(bits) % 8
    if rem != 0:
        bits = np.pad(bits, ((0, 8 - rem),))
    return bits.view(np.uint64)


def bits_to_signs(bits: NDArray[np.uint64], count: int) -> NDArray[np.float64]:
    assert bits.dtype == np.uint64
    n_words = (count + 63) // 64
    if n_words != bits.shape[-1]:
        raise ValueError(
            "'bits' has wrong shape: {}; expected {} elements along the last dimension"
            "".format(bits.shape, n_words)
        )
    signs = np.unpackbits(bits.view(np.uint8), count=count, bitorder="little").astype(np.float64)
    signs = 2 * signs - 1
    return signs


def anneal(
    hamiltonian: Hamiltonian,
    # x0: Optional[NDArray[np.uint64]] = None,
    seed: Optional[int] = None,
    number_sweeps: int = 5000,
    beta0: Optional[float] = None,
    beta1: Optional[float] = None,
    repetitions: int = 1,
    only_best: bool = True,
):
    # tick = time.time()
    if not isinstance(hamiltonian, Hamiltonian):
        raise TypeError("'hamiltonian' must be a Hamiltonian, but got {}".format(type(hamiltonian)))
    number_sweeps = int(number_sweeps)
    if number_sweeps <= 0:
        raise ValueError("'number_sweeps' must be positive, but got {}".format(number_sweeps))
    repetitions = int(repetitions)
    if repetitions <= 0:
        raise ValueError("'repetitions' must be positive, but got {}".format(repetitions))
    if seed is None:
        seed = np.random.randint((1 << 32) - 1, dtype=np.uint32)
    seed = int(seed)

    n_bits = hamiltonian.size
    n_words = (n_bits + 63) // 64

    # if x0 is not None:
    #     x0 = np.ascontiguousarray(np.copy(x0), dtype=np.uint64)
    #     if x0.shape != (n_words,):
    #         raise ValueError("'x0' has wrong shape: {}; expected {}".format(x0.shape, (n_words,)))
    # else:
    #     x0 = signs_to_bits(np.random.choice([-1, 1], size=n_bits))
    exchange = hamiltonian.exchange
    if not isinstance(exchange, scipy.sparse.csr_matrix):
        exchange = exchange.tocsr()

    # exchange = 0.5 * (matrix + matrix.transpose())

    elts = np.asarray(exchange.data, dtype=np.float64)
    cols = np.asarray(exchange.indices, dtype=np.int32)
    rows = np.asarray(exchange.indptr, dtype=np.int32)
    field = np.asarray(hamiltonian.field, dtype=np.float64)

    max_coupling: Optional[float] = None
    if beta0 is None and beta1 is None:
        # Let's rescale the Hamiltonian such that the largest coupling is 1
        max_coupling = float(np.max(np.abs(elts)))
        elts = elts / max_coupling
        field = field / max_coupling

    if beta0 is None or beta1 is None:
        c_beta0 = ffi.new("double *")
        c_beta1 = ffi.new("double *")
        _lib.sa_estimate_betas_f64(
            n_bits,
            ffi.from_buffer("double const[]", elts, require_writable=False),
            ffi.from_buffer("int32_t const[]", cols, require_writable=False),
            ffi.from_buffer("int32_t const[]", rows, require_writable=False),
            ffi.from_buffer("double const[]", field, require_writable=False),
            c_beta0,
            c_beta1,
        )
        if beta0 is None:
            beta0 = float(c_beta0[0])
        if beta1 is None:
            beta1 = float(c_beta1[0])
    # print(beta0, beta1)

    configurations = np.array(
        [signs_to_bits(x) for x in np.random.choice([-1, 1], size=(repetitions, n_bits))]
    )
    energies = np.zeros(repetitions, dtype=np.float64)

    _lib.sa_anneal_f64(
        n_bits,
        ffi.from_buffer("double const[]", elts, require_writable=False),
        ffi.from_buffer("int32_t const[]", cols, require_writable=False),
        ffi.from_buffer("int32_t const[]", rows, require_writable=False),
        ffi.from_buffer("double const[]", field, require_writable=False),
        seed,
        repetitions,
        number_sweeps,
        beta0,
        beta1,
        ffi.from_buffer("uint64_t *", configurations, require_writable=True),
        ffi.from_buffer("double *", energies, require_writable=True),
    )
    # tock = time.time()

    if max_coupling is not None:
        energies *= max_coupling

    for (x, e) in zip(configurations, energies):
        # print(x, hamiltonian.energy(x), e)
        if not np.isclose(hamiltonian.energy(x), e, rtol=1e-10, atol=1e-12):
            raise ValueError(
                "bug in ising_glass_annealer.sa_anneal_f64: e={}, but hamiltonian.energy(x)={}"
                "".format(e, hamiltonian.energy(x))
            )
    if only_best:
        index = np.argmin(energies)
        # logger.debug(
        #     "Completed annealing in {:.1f} seconds. Initial energy: {}; final energy: {}.",
        #     tock - tick,
        #     energy0,
        #     energies[index],
        # )
        return configurations[index], energies[index]

    # logger.debug("Completed annealing in {:.1f} seconds.", tock - tick)
    return configurations, energies
