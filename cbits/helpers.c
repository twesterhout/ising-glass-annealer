#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

int unsafe_get_spin(uint64_t const *bits, unsigned const i) {
  int const block = i / 64;
  int const rest = i % 64;
  return 2 * (int)((bits[block] >> rest) & 1U) - 1;
}

void recompute_energy_changes(double const csr_data[],
                              uint32_t const csr_column_indices[],
                              uint32_t const csr_row_indices[],
                              unsigned const i, uint64_t const *bits,
                              double *energy_changes) {
  uint32_t const begin = csr_row_indices[i];
  uint32_t const end = csr_row_indices[i + 1];
  double const pre = -8 * unsafe_get_spin(bits, i);
  energy_changes[i] *= -1;
  // fprintf(stderr, "Flipping e[%u]\n", i);
  for (uint32_t k = begin; k < end; ++k) {
    uint32_t const j = csr_column_indices[k];
    double const coupling = csr_data[k];
    // fprintf(stderr, "k=%u, j=%u, pre=%f, coupling=%f, e=%f, Δe=%f\n", k, j,
    // pre,
    //         coupling, energy_changes[j],
    //         pre * coupling * unsafe_get_spin(bits, j));
    energy_changes[j] += pre * coupling * unsafe_get_spin(bits, j);
  }
  // abort();
}
