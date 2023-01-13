#include <HsFFI.h>
#include <Rts.h>
#include <stdio.h>

void sa_init(void) {
  int argc = 1;
  char *argv[] = {"ising_glass_annealer", NULL};
  // "+RTS", "-N1", "--install-signal-handlers=no", "-RTS", NULL};
  char **pargv = argv;

  // For some reason, options from argv are not processed properly, so we
  // manually set all RTS options using rts_opts field of RtsConfig
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  conf.rts_opts = "-N --install-signal-handlers=no";
  hs_init_ghc(&argc, &pargv, conf);

  // ls_hs_internal_set_free_stable_ptr(&hs_free_stable_ptr);
}

void sa_exit(void) {
  // LATTICE_SYMMETRIES_LOG_DEBUG("%s", "Calling hs_exit...\n");
  hs_exit();
  // LATTICE_SYMMETRIES_LOG_DEBUG("%s", "Deinitialized RTS!\n");
}