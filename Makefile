.POSIX:
.SUFFIXES:

UNAME = $(shell uname)
ifeq ($(UNAME), Darwin)
  SHARED_EXT = dylib
  CONDA_CC = $(CC)
  CONDA_PREFIX = 
else
  SHARED_EXT = so
  CONDA_CC ?= $(shell conda run -n ci_devel bash -c "which \$${CC}")
  CONDA_PREFIX ?= $(shell conda run -n ci_devel bash -c "echo \$${CONDA_PREFIX}")
endif

.PHONY: conda
conda: haskell
	# conda run -n ci_devel bash -c
	conda build -c conda-forge python/conda

.PHONY: haskell
haskell: cabal.project.local
	cabal build
	find dist-newstyle -name "libising_glass_annealer.$(SHARED_EXT)" \
	  -exec install -m644 -C {} python/ising_glass_annealer/ \;
ifeq ($(UNAME), Linux)
	patchelf --set-rpath '$$ORIGIN' python/ising_glass_annealer/libising_glass_annealer.$(SHARED_EXT)
endif

cabal.project.local:
ifneq ($(CONDA_PREFIX),)
	@echo "Creating cabal.project.local ..."
	@echo "-- DO NOT MODIFY (Generated automatically by Makefile)" >$@
	@echo "package ising-glass-annealer" >>$@
	@echo "  extra-lib-dirs: $(CONDA_PREFIX)/lib" >>$@
	@echo "  ghc-options: -pgmP $(CONDA_CC) -pgmc $(CONDA_CC) -pgma $(CONDA_CC) -pgml $(CONDA_CC)" >>$@
else
	@echo "No Conda found, leaving cabal.project.local untouched ..."
	@touch cabal.project.local
endif

.PHONY: clean
clean:
	rm -f python/ising_glass_annealer/*.so
	cabal clean