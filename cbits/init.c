#include "ising_glass_annealer.h"
#include <HsFFI.h>
#include <stdio.h>

void sa_init(void) {
  int argc = 1;
  char *argv[] = {"-N"};
  hs_init(&argc, (char ***)&argv);
}

void sa_exit(void) { hs_exit(); }

void const *sa_symbol_table[] = {
    &sa_create_hamiltonian, &sa_destroy_hamiltonian, &sa_find_ground_state};
