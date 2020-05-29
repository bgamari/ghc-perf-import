{ mkDerivation, attoparsec, base, base-compat
, base-compat-batteries, base-orphans, base16-bytestring
, bytestring, containers, deepseq, Diff, directory, dlist, filepath
, generic-deriving, ghc-prim, hashable, hashable-time
, integer-logarithms, primitive, QuickCheck, quickcheck-instances
, scientific, stdenv, tagged, tasty, tasty-golden, tasty-hunit
, tasty-quickcheck, template-haskell, text, th-abstraction, time
, time-compat, unordered-containers, uuid-types, vector
}:
mkDerivation {
  pname = "aeson";
  version = "1.4.7.1";
  sha256 = "07e746655fd9bec81c59927c5617877ff4fcd81d0df45c5fb8ef154fb8f40294";
  revision = "1";
  editedCabalFile = "1fih6nmhvg0dvhngk2bjsr9s0804lgng971qz4fjl4mpb7cjz3bd";
  libraryHaskellDepends = [
    attoparsec base base-compat-batteries bytestring containers deepseq
    dlist ghc-prim hashable primitive scientific tagged
    template-haskell text th-abstraction time time-compat
    unordered-containers uuid-types vector
  ];
  testHaskellDepends = [
    attoparsec base base-compat base-orphans base16-bytestring
    bytestring containers Diff directory dlist filepath
    generic-deriving ghc-prim hashable hashable-time integer-logarithms
    QuickCheck quickcheck-instances scientific tagged tasty
    tasty-golden tasty-hunit tasty-quickcheck template-haskell text
    time time-compat unordered-containers uuid-types vector
  ];
  homepage = "https://github.com/bos/aeson";
  description = "Fast JSON parsing and encoding";
  license = stdenv.lib.licenses.bsd3;
}
