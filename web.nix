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
      mkdir -p $out
      install -t $out bundle.js index.html ghc_perf.css spinner.css
    '';
  }
