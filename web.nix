{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
  nodePkg = import ./web { inherit pkgs; };
in
  stdenv.mkDerivation {
    name = "ghc-perf-web";
    src = nodePkg.package;
    sourceRoot = "${nodePkg.package.name}/lib/node_modules/ghc-perf-import";
    installPhase = ''
      mkdir $out
      make install DESTDIR=$out
    '';
  }
