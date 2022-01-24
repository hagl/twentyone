{}:
let
  ghc = import ./nix/ghc.nix;
in
  ghc
    .callCabal2nix "twentyone" ./. {}
