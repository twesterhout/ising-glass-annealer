import h5py
import ising_glass_annealer as sa
import numpy as np
import scipy.sparse


def example(hdf5_file: str = "../test/sa_test_kagome_16.h5", repetitions: int = 4):
    with h5py.File(hdf5_file, "r") as f:
        data = np.asarray(f["elements"])
        indices = np.asarray(f["indices"])
        indptr = np.asarray(f["indptr"])
        field = np.asarray(f["field"])
        energy = float(f["energy"][:])
        signs = np.asarray(f["signs"])
        # offset = np.asarray(f["offset"])
        hamiltonian = sa.Hamiltonian(scipy.sparse.csr_matrix((data, indices, indptr)), field)
        (x, e) = sa.anneal(hamiltonian, repetitions=repetitions, only_best=True)
        assert np.isclose(e, energy)
        print(signs == x)

        print("Ground state energy:", energy)
        print("SA energies:", e)


if __name__ == "__main__":
    example(repetitions=4)
