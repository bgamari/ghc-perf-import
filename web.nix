{ pkgs ? import <nixpkgs> {} }:

with pkgs;
let
  nodePkg = import ./web { inherit pkgs; };
in
  stdenv.mkDerivation {
    name = "ghc-perf-web";
    src = nodePkg.package;
    nativeBuildInputs = [ nodePkg.shell nodePackages.browserify ];
    sourceRoot = "${nodePkg.package.name}/lib/node_modules/ghc-perf-import";
    buildPhase = ''
      browserify ghc_perf.js -o bundle.js
    '';
    installPhase = ''
      mkdir $out
      make install DESTDIR=$out
    '';
  }
