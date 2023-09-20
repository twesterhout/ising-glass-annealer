{
  description = "twesterhout/ising-glass-annealer: Experiments with finding the ground states of Ising spin glasses using (classical) Simulated Annealing";

  nixConfig = {
    extra-substituters = "https://twesterhout-chapel.cachix.org";
    extra-trusted-public-keys = "twesterhout-chapel.cachix.org-1:bs5PQPqy21+rP2KJl+O40/eFVzdsTe6m7ZTiOEE7PaI=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = inputs:
    let
      inherit (inputs.nixpkgs) lib;
      # only consider source dirs and package.yaml as source to our Haskell package
      # this allows the project to rebuild only when source files change, not e.g. readme
      src = inputs.nix-filter.lib {
        root = ./.;
        include = [
          "src"
          "lib"
          "cbits"
          "ising-glass-annealer.cabal"
          "LICENSE"
          "README.md"
        ];
      };

      doEnableRelocatedStaticLibs = ghcVersion: (final: prev:
        # An overlay to replace ghc96 with a custom one that has
        # the static RTS libraries compiled with -fPIC. This lets us use
        # these static libraries to build a self-contained shared library.
        let
          ourGhc = prev.haskell.compiler.${ghcVersion}.override {
            enableRelocatedStaticLibs = true;
          };
        in
        lib.recursiveUpdate prev {
          haskell.compiler.${ghcVersion} = ourGhc;
          haskell.packages.${ghcVersion} =
            prev.haskell.packages.${ghcVersion}.override
              (old: {
                overrides = prev.lib.composeExtensions
                  (old.overrides or (_: _: { }))
                  (hfinal: hprev: {
                    mkDerivation = args: (hprev.mkDerivation args).overrideAttrs (attrs: {
                      configureFlags = (attrs.configureFlags or [ ]) ++ [
                        "--ghc-option=-fPIC"
                        "--ghc-option=-fexternal-dynamic-refs"
                      ];
                    });
                  });
              })
            // { ghc = ourGhc; };
        });

      doInstallForeignLibs = { headers ? [ ] }: drv: drv.overrideAttrs (attrs: {
        # Add lib to the outputs
        outputs =
          let prev = attrs.outputs or [ ];
          in
          if lib.elem "lib" prev then prev else prev ++ [ "lib" ];
        postInstall = ''
          ${attrs.postInstall or ""}

          echo "Installing foreign libraries to $lib/lib ..."
          mkdir -p $lib/lib
          for f in $(find $out/lib/ghc-*/lib -maxdepth 1 -type f -regex '.*\.\(so\|dylib\)'); do
            install -v -Dm 755 "$f" $lib/lib/
          done

          echo "Installing include files to $lib/include ..."
          mkdir -p $out/include
          for f in ${lib.concatStringsSep " " headers}; do
            install -v -Dm 644 "$f" $out/include/
          done
        '';
      });

      ising-glass-annealer-overlay = self: super: {
        haskell = super.haskell // {
          packageOverrides = lib.composeManyExtensions [
            super.haskell.packageOverrides
            (hself: hsuper: {
              ising-glass-annealer =
                doInstallForeignLibs
                  { headers = [ ./cbits/ising_glass_annealer.h ]; }
                  (hself.callCabal2nix "ising-glass-annealer" src { });
            })
            # (hself: hsuper: {
            #   mkDerivation = builtins.trace "enabling profiling" (args: hsuper.mkDerivation (args // { enableLibraryProfiling = true; }));
            # })
          ];
        };
      };

      composed-overlay = { enableProfiling ? false }:
        lib.composeManyExtensions [
          (doEnableRelocatedStaticLibs "ghc962")
          ising-glass-annealer-overlay
        ];

      pkgs-for = args: system: import inputs.nixpkgs {
        inherit system;
        overlays = [ (composed-overlay args) ];
      };

    in
    {
      overlays.default = composed-overlay { };

      packages = inputs.flake-utils.lib.eachDefaultSystemMap (system:
        with (pkgs-for { } system); {
          inherit haskell;
          default = haskell.packages.ghc962.ising-glass-annealer;
          lib = haskell.packages.ghc962.ising-glass-annealer.lib;
        });

      devShells = inputs.flake-utils.lib.eachDefaultSystemMap (system:
        let
          dev-shell-for = pkgs: with pkgs; haskellPackages.shellFor {
            packages = ps: [ ps.ising-glass-annealer ];
            withHoogle = true;
            nativeBuildInputs = with haskellPackages; [
              cabal-install
              cabal-fmt
              fourmolu
              haskell-language-server
              nil
              nixpkgs-fmt
            ];
          };
        in
        {
          default = dev-shell-for (pkgs-for { } system);
          profiling = dev-shell-for (pkgs-for { enableProfiling = true; } system);
        });
    };
}
