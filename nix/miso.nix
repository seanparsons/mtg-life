{ mkDerivation, aeson, base, bytestring, containers, ghcjs-base
, http-api-data, http-types, network-uri, scientific, servant
, stdenv, text, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "miso";
  version = "0.14.0.0";
  sha256 = "dd5668b43f85b4c4835f2f48b01e0b9cb23eac044977de6496925919190f119b";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers ghcjs-base http-api-data
    http-types network-uri scientific servant text transformers
    unordered-containers vector
  ];
  homepage = "http://github.com/dmjio/miso";
  description = "A tasty Haskell front-end framework";
  license = stdenv.lib.licenses.bsd3;
}
