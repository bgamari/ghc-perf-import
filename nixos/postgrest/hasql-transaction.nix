{ mkDerivation, async, base, bytestring, bytestring-tree-builder
, contravariant, contravariant-extras, hasql, mtl, rebase, stdenv
, transformers
}:
mkDerivation {
  pname = "hasql-transaction";
  version = "1.0.0.1";
  sha256 = "149739aba03a0a2c40e965cc3b7b995ae60be95a1f06e26dbca6c82bf552db49";
  libraryHaskellDepends = [
    base bytestring bytestring-tree-builder contravariant
    contravariant-extras hasql mtl transformers
  ];
  testHaskellDepends = [ async hasql rebase ];
  homepage = "https://github.com/nikita-volkov/hasql-transaction";
  description = "Composable abstraction over retryable transactions for Hasql";
  license = stdenv.lib.licenses.mit;
}
