#include <HsFFI.h>
#include <stdint.h>

float wordToFloat(uint64_t const x) {
  float const m_inv_33 = 1.16415321826934814453125e-10;
  float const m_inv_32 = 2.3283064365386962890625e-10;
  int const i = (int)x;
  return ((float)i * m_inv_32) + 0.5f + m_inv_33;
}

static inline int indexBits(uint64_t const *bits, uint32_t const i) {
  unsigned const block = i / 64;
  unsigned const rest = i % 64;
  return -1 + 2 * (int)((bits[block] >> rest) & 0x1);
}

void recomputeEnergyChanges(double const *elements,
                            uint32_t const *columnIndices,
                            uint32_t const *rowIndices, double *deltaEnergies,
                            uint32_t const i, uint64_t const *bits) {
  unsigned const begin = rowIndices[i];
  unsigned const end = rowIndices[i + 1];
  __builtin_prefetch(bits + i / 64);
  for (unsigned k = begin; k < end; ++k) {
    __builtin_prefetch(bits + columnIndices[k] / 64);
  }
  double const pre = -8 * (double)indexBits(bits, i);
  for (unsigned k = begin; k < end; ++k) {
    unsigned const j = columnIndices[k];
    double const coupling = elements[k];
    double const sigma = (double)indexBits(bits, j);
    deltaEnergies[j] += pre * coupling * sigma;
  }
  deltaEnergies[i] *= -1;
}
