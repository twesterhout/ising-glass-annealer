#include <HsFFI.h>
#include <Rts.h>
#include "ising_glass_annealer.h"

void sa_init(void) {
  int argc = 1;
  char *argv[] = {"ising_glass_annealer", NULL};
  char **pargv = argv;

  // For some reason, options from argv are not processed properly, so we
  // manually set all RTS options using rts_opts field of RtsConfig
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  conf.rts_opts = "-N --install-signal-handlers=no";
  hs_init_ghc(&argc, &pargv, conf);
}

void sa_exit(void) {
  hs_exit();
}
