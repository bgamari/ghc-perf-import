{ stdenv, mkDerivation, base, ghcjs-base, aeson, miso, lens, transformers }:
mkDerivation {
  pname = "app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base aeson ghcjs-base miso lens transformers ];
  description = "First miso app";
  license = stdenv.lib.licenses.bsd3;
}
