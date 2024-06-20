{ version
}:

final: prev: {
  pythonPackagesExtensions =
    let haskell-package = prev.haskell.packages.ghc96.ising-glass-annealer.lib;
    in prev.pythonPackagesExtensions ++ [
      (python-final: python-prev: {
        ising-glass-annealer = python-final.buildPythonPackage rec {
          pname = "ising-glass-annealer";
          inherit version;
          src = ./.;

          buildInputs = [ haskell-package ];
          propagatedBuildInputs = with python-final; [
            cffi
            h5py
            loguru
            numpy
            scipy
          ];

          postPatch = ''
            for f in ${haskell-package}/include/*.h; do
              ln --symbolic -v "$f" ising_glass_annealer/
            done
          '';

          preCheck = "rm -rf ising_glass_annealer";

          checkPhase = ''
            runHook preCheck
            python3 -m pytest --color=yes --capture=no run_tests.py
            runHook postCheck
          '';

          preShellHook = ''
            if test -e setup.py; then
              rm -rf build/ ising_glass_annealer/*.{h,so}
              ${postPatch}
            fi
          '';

          nativeCheckInputs = with python-final; [ pip pytestCheckHook ];
        };
      })
    ];
}
