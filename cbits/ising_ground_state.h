#ifndef ISING_GROUND_STATE_H
#define ISING_GROUND_STATE_H

#include <stdint.h>

void sa_init(void);
void sa_exit(void);

void *sa_create_hamiltonian(uint32_t, uint32_t const *, uint32_t const *,
                            double const *, uint32_t, double const *);
void sa_destroy_hamiltonian(void *);

void sa_find_ground_state(void *hamiltonian, uint64_t const *init,
                          uint32_t seed, uint32_t number_sweeps, double beta0,
                          double beta1, uint64_t *configuration,
                          double *energy);

#endif // ISING_GROUND_STATE_H
