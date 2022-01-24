let
  pkgs = import ./pkgs.nix;
  compilerVersion = "ghc8107";
in
  pkgs.haskell.packages."${compilerVersion}"
