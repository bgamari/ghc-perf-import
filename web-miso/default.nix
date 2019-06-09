{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a0aeb23";
    sha256 = "04dgg0f2839c1kvlhc45hcksmjzr8a22q1bgfnrx71935ilxl33d";
  }){}
}:
let
  result = import (pkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    sha256 = "0hmhghnq3hhc6dh530i3mlrz375vfvb9aswvccpr7kbinj8vy6aa";
    rev = "18de471f5ac16e67803a31f72a3028d87df2f0b7";
  }) {};

  app = pkgs.haskell.packages.ghcjsHEAD.callPackage ./app.nix {
    miso = result.miso-ghcjs;
  };

  final = pkgs.runCommand "final" {
    preferLocalBuild = true;
  } ''
    mkdir -p $out
    cp ${app}/bin/app.jsexe/all.js $out
    cp ${./index.html} $out/index.html
    cp ${./style.css} $out/style.css
  '';

in final
