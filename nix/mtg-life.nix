{ mkDerivation, base, miso, stdenv }:
mkDerivation {
  pname = "mtg-life";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base miso ];
  license = stdenv.lib.licenses.mit;
}
