{ mkDerivation, base, Chart, Chart-cairo, lib, simfin }:
mkDerivation {
  pname = "examples";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base Chart Chart-cairo simfin ];
  license = lib.licenses.mit;
}
