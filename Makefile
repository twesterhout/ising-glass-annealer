.POSIX:
.SUFFIXES:

UNAME = $(shell uname)
ifeq ($(UNAME), Darwin)
  SHARED_EXT = dylib
  # CONDA_CC = $(CC)
  # CONDA_PREFIX = 
else
  SHARED_EXT = so
  # CONDA_CC ?= $(shell conda run -n ci_devel bash -c "which \$${CC}")
  # CONDA_PREFIX ?= $(shell conda run -n ci_devel bash -c "echo \$${CONDA_PREFIX}")
endif

.PHONY: conda
conda: haskell
	# conda run -n ci_devel bash -c
	conda build -c conda-forge python/conda

.PHONY: haskell
haskell:
	cabal build -f+use-standalone
	find -D exec dist-newstyle -name "libising_glass_annealer.$(SHARED_EXT)" \
	   -exec install -m644 -C {} python/ising_glass_annealer/ \;
ifeq ($(UNAME), Linux)
	patchelf --set-rpath '$$ORIGIN' python/ising_glass_annealer/libising_glass_annealer.$(SHARED_EXT)
endif

GHC_VERSION := $(shell ghc --version | sed -e 's/[^0-9]*//')
HASKELL_LIBRARY := $(shell find dist-newstyle/ -type f -name "libising_glass_annealer.$(SHARED_EXT)" | grep $(GHC_VERSION))


.PHONY: centos_compile
centos_compile:
	sudo docker run \
		--rm \
		-v $$PWD:/work/ising-glass-annealer \
		--user $$(id -u):$$(id -g) \
		twesterhout/ising-glass-annealer \
		bash -c 'cabal build && rm -r bundle && make bundle'


bundle:
	mkdir -p bundle/lib/haskell
	install -m644 $(HASKELL_LIBRARY) bundle/lib/
	patchelf --set-rpath '$$ORIGIN/haskell' bundle/lib/libising_glass_annealer.$(SHARED_EXT)
	ldd $(HASKELL_LIBRARY) | \
		grep $(SHARED_EXT) | \
		sed -e '/^[\^t]/d' | \
		sed -e 's/\t//' | \
		sed -e 's/ (0.*)//' | \
		grep libHS | \
		sed -e 's/.* => //' | \
		xargs -I '{}' install -m 644 '{}' bundle/lib/haskell/
	find bundle/lib/haskell -type f -exec patchelf --set-rpath '$$ORIGIN' {} \;


cabal.project.local:
ifneq ($(CONDA_PREFIX),)
	@echo "Creating cabal.project.local ..."
	@echo "-- DO NOT MODIFY (Generated automatically by Makefile)" >$@
	@echo "package ising-glass-annealer" >>$@
	@echo "  extra-lib-dirs: $(CONDA_PREFIX)/lib" >>$@
	@echo "  ghc-options: -pgmP $(CONDA_CC) -pgmc $(CONDA_CC) -pgma $(CONDA_CC) -pgml $(CONDA_CC)" >>$@
	@echo "               -ddump-simpl -ddump-stg-final -ddump-cmm -ddump-asm -ddump-to-file" >>$@
	@echo "  flags: +use-standalone -dont-build-shared -build-example" >>$@
	@echo "" >>$@
	@echo "package hdf5-hs" >>$@
	@echo "  extra-include-dirs: $(CONDA_PREFIX)/include" >>$@
	@echo "  extra-lib-dirs: $(CONDA_PREFIX)/lib" >>$@
	@echo "  flags: +disable-default-paths" >>$@
else
	@echo "Creating cabal.project.local ..."
	@echo "-- DO NOT MODIFY (Generated automatically by Makefile)" >$@
	@echo "package ising-glass-annealer" >>$@
	@echo "  ghc-options: -ddump-simpl -ddump-stg-final -ddump-cmm -ddump-asm -ddump-to-file" >>$@
	@echo "  flags: +use-standalone -dont-build-shared -build-example" >>$@
	@echo "" >>$@
	@echo "package hdf5-hs" >>$@
	@echo "  extra-include-dirs: $(CONDA_PREFIX)/include" >>$@
	@echo "  extra-lib-dirs: $(CONDA_PREFIX)/lib" >>$@
endif

.PHONY: clean
clean:
	rm -f python/ising_glass_annealer/*.so
	cabal clean
