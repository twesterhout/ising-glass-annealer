PREFIX = $(PWD)/python/prefix
PROJECT_NAME = ising-glass-annealer
LIBRARY_NAME = $(subst -,_,$(PROJECT_NAME))
VERSION = 0.2.0.0

# We need to run Cabal before trying to guess CABAL_BUILD_DIR etc.
CABAL_OUTPUT = $(shell cabal v2-build)
$(info $(CABAL_OUTPUT))

GHC_VERSION ?= $(shell ghc --numeric-version)
CABAL_BUILD_DIR = $(shell dirname $$(find dist-newstyle/build -name 'libHS$(PROJECT_NAME)*.a' | xargs ls -t | head -n 1))
CABAL_AUTOGEN_DIR = $(CABAL_BUILD_DIR)/global-autogen

HS_LDFLAGS = $(shell cat "$(CABAL_AUTOGEN_DIR)/HS_LIBRARY_PATHS_LIST" | sed 's/^/-L"/;s/$$/"/' | tr '\n' ' ')
HS_LDFLAGS += $(shell cat "$(CABAL_AUTOGEN_DIR)/HS_LIBRARIES_LIST" | sed 's/^/-l/' | tr '\n' ' ')
C_LDFLAGS = $(shell cat "$(CABAL_AUTOGEN_DIR)/EXTRA_LIBRARIES_LIST" | sed 's/^/-l/' | tr '\n' ' ')

PKGCONFIG_FILE = $(PREFIX)/lib/pkgconfig/$(LIBRARY_NAME).pc

all: shared include pkgconfig
shared: $(PREFIX)/lib/lib$(LIBRARY_NAME).so
pkgconfig: $(PKGCONFIG_FILE)
include: $(PREFIX)/include/ising_glass_annealer.h

build/api.txt: cbits/init.c
	mkdir -p build
	cat $< \
		| tr '\n' ' ' \
		| sed -E 's/.*sa_symbol_table\s*\[\]\s*=\s*\{([^}]*)\};.*/\1/' \
		| tr -d '& ' \
		| tr ',' '\n' \
		> $@

$(PREFIX)/include/ising_glass_annealer.h: cbits/ising_glass_annealer.h
	mkdir -p $$(dirname $@)
	install -m 644 $< $@

$(PREFIX)/lib/lib$(LIBRARY_NAME).so: cbits/init.c $(CABAL_BUILD_DIR)/libHS$(PROJECT_NAME)* build/api.txt $(CABAL_AUTOGEN_DIR)/HS_LIBRARY_PATHS_LIST $(CABAL_AUTOGEN_DIR)/HS_LIBRARIES_LIST $(CABAL_AUTOGEN_DIR)/EXTRA_LIBRARIES_LIST
	mkdir -p $$(dirname $@)
	ghc -v --make -no-hs-main -shared -threaded \
		-fPIC -g -O2 -optc-O -fexpose-all-unfoldings -fspecialise-aggressively \
		-pgmP $(CC) -pgmc $(CC) -pgma $(CC) -pgml $(CC) \
		-optl -Wl,--retain-symbols-file=build/api.txt \
		$< -o $@ \
		-L"$(CABAL_BUILD_DIR)" \
		$(HS_LDFLAGS) $(C_LDFLAGS)

$(PKGCONFIG_FILE):
	mkdir -p $$(dirname $(PKGCONFIG_FILE))
	printf "# Generated automatically by Makefile\n"    > "$(PKGCONFIG_FILE)"
	printf "prefix=$(PREFIX)\n"                        >> "$(PKGCONFIG_FILE)"
	printf "libdir=${PREFIX}/lib\n"                    >> "$(PKGCONFIG_FILE)"
	printf "includedir=$(PREFIX)/include\n"            >> "$(PKGCONFIG_FILE)"
	printf "Name: $(LIBRARY_NAME)\n"                   >> "$(PKGCONFIG_FILE)"
	printf "Description: See README\n"                 >> "$(PKGCONFIG_FILE)"
	printf "Version: $(VERSION)\n"                     >> "$(PKGCONFIG_FILE)"
	printf "Libs: -L\$${libdir} -l$(LIBRARY_NAME)\n"   >> "$(PKGCONFIG_FILE)"
	printf "Libs.private:\n"                           >> "$(PKGCONFIG_FILE)"
	printf "Cflags: -I\$${includedir}\n"               >> "$(PKGCONFIG_FILE)"

.PHONY: clean
clean:
	rm -rf build/ cbits/init.o
