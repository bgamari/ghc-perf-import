{ mkDerivation, base-prelude, hasql, hspec, resource-pool, stdenv
, time
}:
mkDerivation {
  pname = "hasql-pool";
  version = "0.5.2";
  sha256 = "9ff2140407f88ca46769069d00314a85e18a7e759de5f7179f6a14854a030751";
  libraryHaskellDepends = [ base-prelude hasql resource-pool time ];
  testHaskellDepends = [ base-prelude hasql hspec ];
  homepage = "https://github.com/nikita-volkov/hasql-pool";
  description = "A pool of connections for Hasql";
  license = stdenv.lib.licenses.mit;
}
