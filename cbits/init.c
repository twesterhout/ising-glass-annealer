#include "ising_glass_annealer.h"
#include <HsFFI.h>
#include <stdio.h>

void sa_init(void) {
  int argc = 2;
  char *argv[] = {"+RTS", "-N", NULL};
  char **pargv = argv;
  hs_init(&argc, &pargv);
}

void sa_exit(void) { hs_exit(); }

void const *sa_symbol_table[] = {&sa_create_hamiltonian,
                                 &sa_destroy_hamiltonian, &sa_find_ground_state,
                                 &sa_anneal};
