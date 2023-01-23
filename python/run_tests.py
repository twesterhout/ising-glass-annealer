import h5py
from pytest import approx
import ising_glass_annealer as sa
import numpy as np
import scipy.sparse


def test_minimal():
    # fmt: off
    matrix = np.array([[0, 2, 3],
                       [2, 0, 1],
                       [3, 1, 0]], dtype=np.float64)
    # fmt: on
    matrix = scipy.sparse.csr_matrix(matrix)
    field = np.zeros(3, dtype=np.float64)

    h = sa.Hamiltonian(matrix, field)
    x = np.array([-1, -1, 1])
    e = h.energy(sa.signs_to_bits(x))
    assert e == approx(np.dot(x, matrix @ x))


def test_energy():
    np.random.seed(56)
    for i in range(10):
        n = 200
        matrix = scipy.sparse.random(n, n, density=0.1, format="csr", dtype=np.float64)
        matrix = 0.5 * (matrix + matrix.transpose())
        field = np.random.rand(n)

        h = sa.Hamiltonian(matrix, field)
        x = np.random.choice([-1, 1], size=n)
        e = h.energy(sa.signs_to_bits(x))
        assert e == approx(np.dot(x, matrix @ x) + np.dot(x, field))


def test_optimization():
    np.random.seed(42)
    for n in [5, 8, 10, 12]:
        for _ in range(10):
            matrix = scipy.sparse.random(n, n, density=0.3, format="csr", dtype=np.float64)
            matrix = 0.5 * (matrix + matrix.transpose())
            field = 0.1 * np.random.rand(n)

            h = sa.Hamiltonian(matrix, field)
            (x, e) = sa.anneal(h, repetitions=4, number_sweeps=1000, only_best=True)

            x_best = np.array([0], dtype=np.uint64)
            e_best = h.energy(x_best)
            for x_tmp in range(2**n):
                e_tmp = h.energy(np.array([x_tmp], dtype=np.uint64))
                if e_tmp < e_best:
                    x_best[0] = x_tmp
                    e_best = e_tmp

            assert e == approx(e_best)
            assert x == x_best


def test_kagome16(hdf5_file: str = "sa_test_kagome_16.h5", repetitions: int = 4):
    with h5py.File(hdf5_file, "r") as f:
        data = np.asarray(f["elements"])
        indices = np.asarray(f["indices"])
        indptr = np.asarray(f["indptr"])
        field = np.asarray(f["field"])
        energy = np.asarray(f["energy"]).item()
        exact = np.asarray(f["signs"])
        hamiltonian = sa.Hamiltonian(scipy.sparse.csr_matrix((data, indices, indptr)), field)
        (x, e) = sa.anneal(hamiltonian, repetitions=repetitions, only_best=True)
        assert e == approx(energy, rel=1e-8)
        predicted_signs = sa.bits_to_signs(x, count=hamiltonian.size)
        expected_signs = sa.bits_to_signs(exact, count=hamiltonian.size)
        accuracy = np.mean(predicted_signs == expected_signs)
        if accuracy < 0.5:
            accuracy = 1 - accuracy
        assert accuracy > 0.99


def test_greedy_kagome16(hdf5_file: str = "sa_test_kagome_16.h5"):
    with h5py.File(hdf5_file, "r") as f:
        data = np.asarray(f["elements"])
        indices = np.asarray(f["indices"])
        indptr = np.asarray(f["indptr"])
        field = np.asarray(f["field"])
        energy = np.asarray(f["energy"]).item()
        exact = np.asarray(f["signs"])
        hamiltonian = sa.Hamiltonian(scipy.sparse.csr_matrix((data, indices, indptr)), field)
        (x, e) = sa.greedy_solve(hamiltonian)
        predicted_signs = sa.bits_to_signs(x, count=hamiltonian.size)
        expected_signs = sa.bits_to_signs(exact, count=hamiltonian.size)
        accuracy = np.mean(predicted_signs == expected_signs)

        if accuracy < 0.5:
            accuracy = 1 - accuracy
        assert accuracy == 1.0
        assert e == approx(energy, rel=1e-14)
