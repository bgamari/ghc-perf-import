args:
{ lib, ... }:

let
  pkgs = import (fetchGit {
    url = "https://github.com/NixOS/nixpkgs";
    rev = "fc60ed1fffb22e3b001eb65ef73e6899ff180efb";
    ref = "release-20.03";
  }) {};
in
  import ./ghc-perf.nix args { inherit pkgs lib; }
