{ system ? builtins.currentSystem, config ? {} }:

with {
  owner = "MercuryTechnologies";
  repo = "pinned-nixpkgs";
  rev = "8edbd1b94033fcbb502838ca7be158c645a004ca";
};

import (builtins.fetchGit {
  url = "git@github.com:${owner}/${repo}";
  inherit rev;
}) { inherit system config; }
