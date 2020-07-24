{ system ? builtins.currentSystem
, compiler ? "ghc865"
, ...
}:
rec {
  pkgs = import ./.nix/nixpkgs.nix { inherit system compiler; };

  hsPkgs = pkgs.haskell.packages.${compiler};

  taut = hsPkgs.callCabal2nix "taut" ./. {};
}
