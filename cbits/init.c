#include "ising_ground_state.h"
#include <HsFFI.h>
#include <stdio.h>

void sa_init(void) { hs_init(NULL, NULL); }

void sa_exit(void) { hs_exit(); }

void const *const *dummy_keep_symbols(void) {
  static void const *symbols[] = {
      &sa_create_hamiltonian, &sa_destroy_hamiltonian, &sa_find_ground_state};
  return symbols;
}
