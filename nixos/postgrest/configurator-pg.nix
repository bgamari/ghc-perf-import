{ mkDerivation, base, bytestring, containers, filepath, HUnit
, megaparsec, protolude, scientific, stdenv, test-framework
, test-framework-hunit, text
}:
mkDerivation {
  pname = "configurator-pg";
  version = "0.2.3";
  sha256 = "ca4d2dc649ab1d1d51db25aaa3028f77c99a98780a81a72cf2bd6aa2be287a6d";
  libraryHaskellDepends = [
    base containers megaparsec protolude scientific text
  ];
  testHaskellDepends = [
    base bytestring filepath HUnit protolude test-framework
    test-framework-hunit text
  ];
  homepage = "https://github.com/robx/configurator-pg";
  description = "Reduced parser for configurator-ng config files";
  license = stdenv.lib.licenses.bsd3;
}
