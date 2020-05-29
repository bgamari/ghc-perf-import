{ mkDerivation, aeson, aeson-qq, ansi-wl-pprint, async, auto-update
, base, base64-bytestring, bytestring, case-insensitive, cassava
, configurator-pg, containers, contravariant, contravariant-extras
, cookie, directory, either, gitrev, hasql, hasql-pool
, hasql-transaction, heredoc, hspec, hspec-wai, hspec-wai-json
, HTTP, http-types, insert-ordered-containers
, interpolatedstring-perl6, jose, lens, lens-aeson, monad-control
, network, network-uri, optparse-applicative, parsec, process
, protolude, Ranged-sets, regex-tdfa, retry, scientific, stdenv
, swagger2, text, time, transformers-base, unix
, unordered-containers, vector, wai, wai-cors, wai-extra
, wai-middleware-static, warp
}:
mkDerivation {
  pname = "postgrest";
  version = "7.0.1";
  sha256 = "eee92b13da69555fd5132035ab007a6d780a565ac40ca5ab477e6c67634bc6b2";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint base base64-bytestring bytestring
    case-insensitive cassava configurator-pg containers contravariant
    contravariant-extras cookie either gitrev hasql hasql-pool
    hasql-transaction heredoc HTTP http-types insert-ordered-containers
    interpolatedstring-perl6 jose lens lens-aeson network-uri
    optparse-applicative parsec protolude Ranged-sets regex-tdfa
    scientific swagger2 text time unordered-containers vector wai
    wai-cors wai-extra wai-middleware-static
  ];
  executableHaskellDepends = [
    auto-update base base64-bytestring bytestring directory either
    hasql hasql-pool hasql-transaction network protolude retry text
    time unix wai warp
  ];
  testHaskellDepends = [
    aeson aeson-qq async auto-update base base64-bytestring bytestring
    case-insensitive cassava containers contravariant hasql hasql-pool
    hasql-transaction heredoc hspec hspec-wai hspec-wai-json http-types
    lens lens-aeson monad-control process protolude regex-tdfa text
    time transformers-base wai wai-extra
  ];
  homepage = "https://postgrest.org";
  description = "REST API for any Postgres database";
  license = stdenv.lib.licenses.mit;
}
