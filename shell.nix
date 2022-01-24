{}:
let
  pkgs = import ./nix/pkgs.nix;
  ghc = import ./nix/ghc.nix;
  inputs = import ./default.nix {};
in
  with ghc;
  pkgs.mkShell {
    buildInputs = [
      cabal-install
      haskell-language-server
      hlint
      ormolu
      cabal-fmt
    ];
    inputsFrom = [ inputs.env ];
  }
