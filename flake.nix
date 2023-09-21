{
  description = "twesterhout/ising-glass-annealer: Experiments with finding the ground states of Ising spin glasses using (classical) Simulated Annealing";

  nixConfig = {
    extra-substituters = "https://twesterhout-chapel.cachix.org";
    extra-trusted-public-keys = "twesterhout-chapel.cachix.org-1:bs5PQPqy21+rP2KJl+O40/eFVzdsTe6m7ZTiOEE7PaI=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    haskell-python-tools = {
      url = "github:twesterhout/haskell-python-tools.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    let
      inherit (inputs.nixpkgs) lib;
      inherit (inputs.haskell-python-tools.lib)
        doInstallForeignLibs
        doEnableRelocatedStaticLibs;

      version = "0.4.1.2";

      haskell-overlay = self: super: {
        haskell = super.haskell // {
          packageOverrides = lib.composeManyExtensions [
            super.haskell.packageOverrides
            (hself: hsuper: {
              ising-glass-annealer =
                doInstallForeignLibs
                  {
                    headers = [
                      "cbits/ising_glass_annealer.h"
                      "cbits/ising_glass_annealer_declarations.h"
                    ];
                  }
                  (hself.callCabal2nix "ising-glass-annealer" ./haskell { });
            })
          ];
        };
      };
      python-overlay = import ./python/overlay.nix { inherit version; };

      composed-overlay = { enableProfiling ? false }:
        lib.composeManyExtensions [
          (doEnableRelocatedStaticLibs "ghc962")
          haskell-overlay
          python-overlay
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
          python = python3Packages.ising-glass-annealer;
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
          python = with (pkgs-for { } system); python3Packages.ising-glass-annealer.overrideAttrs (attrs: {
            propagatedBuildInputs = (attrs.propagatedBuildInputs or [ ]) ++ [
              python3Packages.h5py
            ];
            nativeBuildInputs = (attrs.nativeBuildInputs or [ ]) ++ [
              python3Packages.black
              nodePackages.pyright
            ];
          });
        });
    };
}
