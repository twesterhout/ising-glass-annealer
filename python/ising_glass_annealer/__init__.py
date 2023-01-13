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

__version__ = "0.4.0.0"
__author__ = "Tom Westerhout <14264576+twesterhout@users.noreply.github.com>"

import os
import sys
import time
import weakref
from typing import Optional

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
    """
    )
    return ffi.dlopen(os.path.join(get_package_path(), get_library_name()))


_lib = load_shared_library()
_lib.sa_init()
_finalizer = weakref.finalize(_lib, _lib.sa_exit)

# def _create_hamiltonian(exchange, field):
#     if not isinstance(exchange, scipy.sparse.spmatrix):
#         raise TypeError("'exchange' must be a sparse matrix, but got {}".format(type(exchange)))
#     if not isinstance(exchange, scipy.sparse.coo_matrix):
#         warnings.warn(
#             "ising_ground_state.anneal works with sparse matrices in COO format, but 'exchange' is "
#             "not. A copy of 'exchange' will be created with proper format. This might incur some "
#             "performance overhead."
#         )
#         exchange = scipy.sparse.coo_matrix(exchange)
#
#     field = np.asarray(field, dtype=np.float64, order="C")
#     if field.ndim != 1:
#         raise ValueError(
#             "'field' must be a vector, but got a {}-dimensional array".format(field.ndim)
#         )
#     if exchange.shape != (len(field), len(field)):
#         raise ValueError(
#             "dimensions of 'exchange' and 'field' do not match: {} vs {}".format(
#                 exchange.shape, len(field)
#             )
#         )
#
#     row_indices = np.asarray(exchange.row, dtype=np.uint32, order="C")
#     column_indices = np.asarray(exchange.col, dtype=np.uint32, order="C")
#     elements = np.asarray(exchange.data, dtype=np.float64, order="C")
#     return _lib.sa_create_hamiltonian(
#         exchange.nnz,
#         row_indices.ctypes.data_as(POINTER(c_uint32)),
#         column_indices.ctypes.data_as(POINTER(c_uint32)),
#         elements.ctypes.data_as(POINTER(c_double)),
#         len(field),
#         field.ctypes.data_as(POINTER(c_double)),
#     )


class Hamiltonian:
    def __init__(self, exchange: scipy.sparse.spmatrix, field: NDArray[np.uint64]):
        # self._payload = _create_hamiltonian(exchange, field)
        # self._finalizer = weakref.finalize(self, _lib.sa_destroy_hamiltonian, self._payload)
        # self.shape = exchange.shape
        # self.dtype = np.float64
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
    assert np.all(mask | signs == -1)
    bits = np.packbits(signs, axis=-1, bitorder="little")
    rem = len(bits) % 8
    if rem != 0:
        bits = np.pad(bits, ((0, 8 - rem),))
    return bits.view(np.uint64)


def example01():
    # fmt: off
    matrix = np.array([[0, 2, 3],
                       [2, 0, 1],
                       [3, 1, 0]], dtype=np.float64)
    # fmt: on
    matrix = scipy.sparse.csr_matrix(matrix)
    field = np.zeros(3, dtype=np.float64)

    h = Hamiltonian(matrix, field)
    x = np.array([-1, -1, 1])
    e = h.energy(signs_to_bits(x))
    print(np.dot(x, matrix @ x))
    print(e)


def example02():
    n = 200
    matrix = scipy.sparse.random(n, n, density=0.1, format="coo", dtype=np.float64)
    matrix.setdiag(np.zeros(n))
    matrix = 0.5 * (matrix + matrix.transpose())
    matrix.eliminate_zeros()
    field = np.random.rand(n)

    h = Hamiltonian(matrix, field)
    x = np.random.choice([-1, 1], size=n)
    e = h.energy(signs_to_bits(x))
    print(np.dot(x, matrix @ x) + np.dot(x, field))
    print(e)


def example03():
    np.random.seed(42)
    n = 10
    matrix = scipy.sparse.random(n, n, density=0.3, format="coo", dtype=np.float64)
    matrix.setdiag(np.zeros(n))
    matrix = 0.5 * (matrix + matrix.transpose())
    matrix.eliminate_zeros()
    field = np.zeros(n)
    # np.random.rand(n)

    h = Hamiltonian(matrix, field)
    (e, x) = anneal(h, repetitions=2, number_sweeps=1000, beta0=1e-2, beta1=1e0, only_best=True)
    print(e, x)

    x_best = np.array([0], dtype=np.uint64)
    e_best = h.energy(x_best)
    for x in range(2**n):
        e = h.energy(np.array([x], dtype=np.uint64))
        if e < e_best:
            x_best[0] = x
            e_best = e
    print(x_best, e_best)
    # x = np.random.choice([-1, 1], size=n)
    # e = h.energy(signs_to_bits(x))
    # print(np.dot(x, matrix @ x) + np.dot(x, field))
    # print(e)


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
    tick = time.time()
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
    elts = np.asarray(exchange.data, dtype=np.float64)
    cols = np.asarray(exchange.indices, dtype=np.int32)
    rows = np.asarray(exchange.indptr, dtype=np.int32)
    field = np.asarray(hamiltonian.field, dtype=np.float64)

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
    tock = time.time()

    for (x, e) in zip(configurations, energies):
        # print(x, hamiltonian.energy(x), e)
        assert np.isclose(hamiltonian.energy(x), e, rtol=1e-10, atol=1e-12)
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
