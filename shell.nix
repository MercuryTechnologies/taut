with (import ./default.nix {});

hsPkgs.shellFor {
  packages = _: [ taut ];

  withHoogle = false;

  buildInputs = with pkgs; [
    cabal-install
    cabal2nix
    hsPkgs.ghcid
  ];
}
