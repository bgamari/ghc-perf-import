{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, attoparsec, base, bytestring
      , containers, dlist, filepath, lens, lens-regex-pcre, lzma
      , optparse-applicative, pcre-light, postgresql-simple, process
      , regex-compat, servant, servant-server, stdenv, template-haskell
      , text, time, transformers, wai, warp, cassava
      }:
      mkDerivation {
        pname = "perf-import";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytestring containers filepath lens lens-regex-pcre pcre-light
          postgresql-simple process template-haskell text time cassava
        ];
        executableHaskellDepends = [
          aeson attoparsec base bytestring containers dlist filepath lzma
          optparse-applicative postgresql-simple process regex-compat servant
          servant-server template-haskell text time transformers wai warp
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  lens-regex-pcre = haskellPackages.callHackage "lens-regex-pcre" "0.3.1.0" {};

  drv = variant (haskellPackages.callPackage f { inherit lens-regex-pcre; });

in

  if pkgs.lib.inNixShell then drv.env else drv
