#include "ising_glass_annealer.h"
#include <HsFFI.h>
#include <Rts.h>
#include <stdio.h>

void sa_init(void) {
  int argc = 3;
  char *argv[] = {"+RTS", "-N", "-RTS", NULL};
  char **pargv = argv;
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  hs_init_ghc(&argc, &pargv, conf);
  // hs_init(&argc, &pargv);
}

void sa_exit(void) { hs_exit(); }

void const *sa_symbol_table[] = {&sa_create_hamiltonian,
                                 &sa_destroy_hamiltonian, &sa_compute_energy,
                                 &sa_find_ground_state, &sa_anneal};
