{ nixpkgs ? (import <nixpkgs> {}) }:

nixpkgs.symlinkJoin {
  name = "ghc-perf-import-all";
  paths = [
    (nixpkgs.python37Packages.callPackage ./package.nix {})
    (import ../import { inherit nixpkgs; })
  ];
}

