{ system ? builtins.currentSystem
, compiler ? "ghc865"
}:

let
primary = import ./pinned-nixpkgs.nix {
  inherit system;
  config = {
    allowUnfree = true;
    packageOverrides = super: let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packages = super.haskell.packages // {
          "${compiler}" = super.haskell.packages.${compiler}.override {
            overrides = hself: hsuper:
              with super.haskell.lib;
              let markUnbroken = old: overrideCabal old (_: { broken = false; }); in rec {
                slack-web = hself.callCabal2nix "slack-web" (super.fetchFromGitHub {
                  owner = "jpvillaisaza";
                  repo = "slack-web";
                  rev = "0ad198a840ca370d998017ebe5bb061c94d213d1";
                  sha256 = "1mkhy4nmyf1vx7g2yizs2nn10lc3nzajz8qcm6wb5zhsga2wggk4";
                }) {};
            }; # END overrides
          }; # END ${compiler}
        }; # END packages
      }; # END haskell
    }; # END packageOverrides
  }; # END config
};

in primary.stable.current
