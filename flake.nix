{
  description = "twesterhout/ising-glass-annealer: Experiments with finding the ground states of Ising spin glasses using (classical) Simulated Annealing";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      # don't look for a flake.nix file in this repository
      # this tells Nix to retrieve this input as just source code
      flake = false;
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      # this style makes it easy to override non-Haskell packages, e.g. to patch them
      # pkgs = import inputs.nixpkgs { inherit system; overlays = []; };
      pkgs = inputs.nixpkgs.legacyPackages.${system};

      # only consider source dirs and package.yaml as source to our Haskell package
      # this allows the project to rebuild only when source files change, not e.g. readme
      src = inputs.nix-filter.lib {
        root = ./.;
        include = [
          "src"
          "lib"
          "cabal.project"
          "cabal.project.local"
          "ising-glass-annealer.cabal"
        ];
      };

      # This allows us to build a Haskell package with any given GHC version.
      # It will also affects all dependent libraries.
      # overrides allows us to patch existing Haskell packages, or introduce new ones
      # see here for specifics: https://nixos.wiki/wiki/Overlays
      haskellPackagesFor =
        { ghcVersion
        , haskellPackages ? pkgs.haskell.packages."ghc${ghcVersion}"
        }:
        haskellPackages.override {
          overrides = self: super: {
            ising-glass-annealer = self.callCabal2nix "ising-glass-annealer" ./. { };
          };
        };

      # A list of GHC versions and corresponding package overrides to use with `haskellPackagesFor`.
      configurations = [
        {
          ghcVersion = "90";
          haskellPackages = inputs.nixpkgs-stable.legacyPackages.${system}.haskellPackages;
        }
        { ghcVersion = "92"; }
        { ghcVersion = "94"; }
      ];


      # A utility function that creates a set containing key-value pairs constructed for each
      # element in `configurations`.
      foldConfigurations = f:
        builtins.foldl'
          (acc: conf:
            acc // { "ghc${conf.ghcVersion}" = f (haskellPackagesFor conf); }
          )
          { }
          configurations;

      # The version of GHC used for default package and development shell.
      defaultGhcVersion = "ghc92";
    in
    rec {
      packages = {
        # Build ising-glass-annealer for one given GHC versions.
        ising-glass-annealer = foldConfigurations (haskellPackages: haskellPackages.ising-glass-annealer);
        default = packages.ising-glass-annealer.${defaultGhcVersion};
      };

      # Prepare a development shell for many diffent GHC versions.
      devShells = foldConfigurations
        (haskellPackages:
          haskellPackages.shellFor {
            packages = ps: [ ps.ising-glass-annealer ];
            nativeBuildInputs = with haskellPackages; [
              cabal-install
              ormolu
              haskell-language-server
            ];
            shellHook = ''
              export PS1="❄️ GHC ${haskellPackages.ghc.version} $PS1"
            '';
          }
        ) // {
        default = devShells.${defaultGhcVersion};
      };

      # The formatter to use for .nix files (but not .hs files)
      # Allows us to run `nix fmt` to reformat nix files.
      formatter = pkgs.nixpkgs-fmt;
    }
  );
}
