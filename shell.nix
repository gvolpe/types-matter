let
  # unstable packages on May 13th
  pkgs = import <nixpkgs> (fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/6bcb1dec8ea.tar.gz") {};
  inherit (pkgs) haskellPackages;
  drv = haskellPackages.callCabal2nix "types-matter" ./. {};

in
  {
    my_project = drv;
    shell = haskellPackages.shellFor {
      withHoogle = true;
      packages = p: [drv];
      buildInputs = with haskellPackages; [ cabal-install hlint ghcide ];
      shellHook = ''
        export NIX_GHC="$(which ghc)"
        export NIX_GHCPKG="$(which ghc-pkg)"
        export NIX_GHC_DOCDIR="$NIX_GHC/../../share/doc/ghc/html"
        export NIX_GHC_LIBDIR="$(ghc --print-libdir)"
      '';
    };
  }.shell
