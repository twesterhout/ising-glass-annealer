import h5py
import ising_glass_annealer
import scipy.sparse


def example(hdf5_file: str = "../../app/problem_kagome_16.h5", repetitions: int = 4):
    with h5py.File(hdf5_file, "r") as f:
        row = f["row_indices"][:]
        col = f["col_indices"][:]
        data = f["elements"][:]
        field = f["field"][:]
        energy = f["energy"][()]
        print("Ground state energy:", energy)
        hamiltonian = ising_glass_annealer.Hamiltonian(
            scipy.sparse.coo_matrix((data, (row, col))), field
        )
        (x, e) = ising_glass_annealer.anneal(hamiltonian, repetitions=repetitions, only_best=False)
        print("SA energies:", e)

if __name__ == '__main__':
    example(repetitions=1)
    example(repetitions=4)
